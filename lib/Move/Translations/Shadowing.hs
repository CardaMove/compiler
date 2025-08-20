module Move.Translations.Shadowing (removeShadowingInModule, removeShadowingInExpr, removeShadowingInSequence, getUnshadowedName, getBindIdentifiers, generateUnshadowedName) where

import Data.Generics.Uniplate.Data (Uniplate (descend))
import Data.List (mapAccumL)
import Data.Map qualified as Map
import Move.AST

-- | Keeps track of a scope. A scope is a map from an identifier to its unshadowed name
type Scope = Map.Map Identifier Identifier

-- |
-- Given an identifier, returns the unshadowed name by examining the scopes from the innermost (topmost) to the outermost (bottommost)
--
-- Raises an error if no entry is found
getUnshadowedName :: Identifier -> [Scope] -> Identifier
getUnshadowedName from [] = error $ "Cannot retrieve identifier from the scopes: " ++ show from
getUnshadowedName from (x : xs) = case Map.lookup from x of
  Just to -> to
  Nothing -> getUnshadowedName from xs

-- | Helper function for `getBindIdentifiers`
getBindIdentifiersHelper :: [BindedField] -> [Identifier]
getBindIdentifiersHelper = concatMap f
  where
    f (BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing}) = [bindFieldIdentifier]
    f (BindedField {bindFieldInnerBind = Just innerBind}) = getBindIdentifiers innerBind

-- |
-- Given a single bind, returns all its binded identifiers as they are named in the AST
getBindIdentifiers :: Bind -> [Identifier]
getBindIdentifiers (BindIdentifier ident) = [ident]
getBindIdentifiers (BindNamedStruct (BindedNamedStruct {bnsFields = BindedFields {bindedFields}})) = getBindIdentifiersHelper bindedFields
getBindIdentifiers (BindPositionalStruct (BindedPositionalStruct {bpsFields = BindedFields {bindedFields}})) = getBindIdentifiersHelper bindedFields

-- |
-- Given an identifier, returns a new name where no shadowing happens.
--
-- The input scopes are checked, starting from the outermost (bottommost) and "_inner" is appended until no shadowing happens.
--
-- The local scope should not be passed to this function since if the name clashes with an identifier on the local scope it would be a normal rebind
generateUnshadowedName :: Identifier -> [Scope] -> Identifier
generateUnshadowedName ident outerScopes = foldr f ident outerScopes
  where
    -- Note: It's always the input identifier that needs to be checked in the scope, not the appended ones
    -- The accumulator grows with _inner whenever the input identifier is found, until the entire list is scanned
    -- In other words: count in how many scopes the input identifier is present and append _inner that number of times
    f scope (Identifier identName) = if Map.member ident scope then Identifier (identName ++ "_inner") else Identifier identName

-- |
-- Helper for `updateBindWithUnshadowedIdentifiers`. Works on fields of any struct
updateBindWithUnshadowedIdentifiersHelper :: BindedField -> [Scope] -> BindedField
updateBindWithUnshadowedIdentifiersHelper (BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing}) scopes = BindedField {bindFieldIdentifier = getUnshadowedName bindFieldIdentifier scopes, bindFieldInnerBind = Nothing}
updateBindWithUnshadowedIdentifiersHelper (BindedField {bindFieldIdentifier, bindFieldInnerBind = Just bindFieldInnerBind}) scopes = BindedField {bindFieldIdentifier, bindFieldInnerBind = Just $ updateBindWithUnshadowedIdentifiers bindFieldInnerBind scopes}

-- |
-- Given a single bind, replaces its binded identifiers with the newly unshadowed names
updateBindWithUnshadowedIdentifiers :: Bind -> [Scope] -> Bind
--    For a single identifier, replace it
updateBindWithUnshadowedIdentifiers (BindIdentifier ident) scopes = BindIdentifier $ getUnshadowedName ident scopes
--    For named structs, inspect its binded fields
updateBindWithUnshadowedIdentifiers (BindNamedStruct struct@BindedNamedStruct {bnsFields = fields@BindedFields {bindedFields}}) scopes =
  BindNamedStruct $ struct {bnsFields = fields {bindedFields = map (`updateBindWithUnshadowedIdentifiersHelper` scopes) bindedFields}}
--    Similar for positional structs
updateBindWithUnshadowedIdentifiers (BindPositionalStruct struct@BindedPositionalStruct {bpsFields = fields@BindedFields {bindedFields}}) scopes =
  BindPositionalStruct $ struct {bpsFields = fields {bindedFields = map (`updateBindWithUnshadowedIdentifiersHelper` scopes) bindedFields}}

-- |
-- Given an expression, removes any shadowing that happens in its scope and inner scopes
--
-- Must be invoked with at least one existing scope since being this an expression, it must belong to a scope
removeShadowingInExpr :: Expr -> [Scope] -> Expr
--    When a local identifier is found, replace with its unshadowed name.
--    This is the base case for the recursion
removeShadowingInExpr (NameAccessChainExpr (LocalNameAccessChain ident)) scopes = NameAccessChainExpr $ LocalNameAccessChain $ getUnshadowedName ident scopes
--    When a Sequence is found, push a new (empty) scope on top of the existing ones and search for bindings
removeShadowingInExpr (SequenceExpr sequence') scopes = SequenceExpr $ removeShadowingInSequence sequence' (Map.empty : scopes)
--    For any other expression type, descend the AST and proceed recursively
removeShadowingInExpr expr scopes = descend (`removeShadowingInExpr` scopes) expr

-- |
-- Version of `removeShadowingInExpr` working with Maybe
removeShadowingInExprMaybe :: Maybe Expr -> [Scope] -> Maybe Expr
removeShadowingInExprMaybe mExpr scopes = fmap (`removeShadowingInExpr` scopes) mExpr -- infix for (\expr -> removeShadowingInExpr expr scopes)

-- |
-- Given a Use, returns all the alias identifiers.
--
-- Also handles the use members and the Self member
getUseIdentifiers :: Use -> [Identifier]
getUseIdentifiers use = case use of
  Use {useIdentifier, useAlias, useMembers = []} -> [getIdentifier useAlias useIdentifier]
  Use {useIdentifier, useMembers = [UseMember {useMemberIdentifier = Identifier "Self", useMemberUseAlias}]} -> [getIdentifier useMemberUseAlias useIdentifier]
  Use {useMembers = [UseMember {useMemberIdentifier, useMemberUseAlias}]} -> [getIdentifier useMemberUseAlias useMemberIdentifier]
  use'@Use {useMembers = x : xs} -> getUseIdentifiers use' {useMembers = [x]} ++ getUseIdentifiers use' {useMembers = xs}
  where
    getIdentifier :: Maybe Identifier -> Identifier -> Identifier
    getIdentifier Nothing def = def
    getIdentifier (Just ident) _ = ident

-- |
-- Given a Sequence, adds a new scope and proceeds by renaming identifiers that cause shadowing,
--
-- while also collecting new identifiers as they are declared in the local scope or found in the Use declarations local to this sequence.
--
-- Must be called with at least one scope. In fact, the topmost scope will be considered the local one.
--
-- This is useful when the Sequence is a function body, since the function parameters have to be on the local scope
--
-- Note: uses and aliases will not be modified, since in any case they cannot be reassigned.
--    This means they can still shadow other identifiers, so further bindings with same identifiers will be unshadowed
--
-- Calls `removeShadowingInExpr` when it encounters an expression
removeShadowingInSequence :: Sequence -> [Scope] -> Sequence
removeShadowingInSequence _ [] = error "Cannot have no scopes in Sequence"
removeShadowingInSequence (Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) (localScope : outerScopes) =
  let usesIdents = map (\ident -> (ident, ident)) $ concatMap getUseIdentifiers sequenceUses
      -- Uses can shadow other identifiers, but since it is not possible to assign a value to a use alias,
      -- there is no need to prevent shadowing for them. So, just add their identifiers to the local scope
      localScope' = Map.union (Map.fromList usesIdents) localScope

      (scopes'', sequenceItems') = mapAccumL f (localScope' : outerScopes) sequenceItems
        where
          -- This case is already handled by the outer pattern match
          f [] _ = error "Cannot have no scopes in Sequence"
          -- If the SequenceItem is an expression, recursively remove shadowing
          f scopes (SequenceItemExpr expr) = (scopes, SequenceItemExpr $ removeShadowingInExpr expr scopes)
          -- If the SequenceItem is a let binding:
          f scopes@(localScope'' : outerScopes') (SequenceItemBindExpr (Bindings {bindings, bindingsBindType, bindingsBindExpr})) = do
            -- First, retrieve all the identifiers that have been binded
            let newIdentifiers = concatMap getBindIdentifiers bindings
            -- Since some of them can shadow other existing identifiers (declared on the outer scopes), generate new names for them
            -- Note that normal rebindings are permitted, so the local scope is not passed to `generateUnshadowedName`
            let unshadowedIdentifiers = map (\ident -> (ident, generateUnshadowedName ident outerScopes')) newIdentifiers
            -- Then create a map from them
            let newBindings = Map.fromList unshadowedIdentifiers

            ( -- The newly binded identifiers are added to the local scope
              Map.union newBindings localScope'' : outerScopes',
              -- The let binding itself is updated so to reflect the new names for the identifiers
              -- The right value of the bind is also recursively unshadowed, but without the newly introduced bindings
              SequenceItemBindExpr $
                Bindings
                  { bindings = map (`updateBindWithUnshadowedIdentifiers` [newBindings]) bindings, -- Infix for (\bind -> updateBindWithUnshadowedIdentifiers bind [newBindings])
                    bindingsBindType,
                    bindingsBindExpr = removeShadowingInExprMaybe bindingsBindExpr scopes
                  }
              )

      sequenceEndExpr' = removeShadowingInExprMaybe sequenceEndExpr scopes''
   in Sequence
        { sequenceUses,
          sequenceItems = sequenceItems',
          sequenceEndExpr = sequenceEndExpr'
        }

-- |
-- Given a Module, first collects all identifiers belonging to either constants and uses present in this module,
-- then proceeds by inspecting all function declarations to remove shadowings
-- 
-- Note: uses and aliases will not be modified, since in any case they cannot be reassigned.
--    This means they can still shadow other identifiers, so further bindings with same identifiers will be unshadowed
removeShadowingInModule :: Module -> Module
removeShadowingInModule currModule@Module {moduleTopLevels} =
  -- All the constants and uses declared in this module will act as a base scope for the module
  let constantIdentifiers = [constantIdentifier | TopLevelConstant (Constant {constantIdentifier}) <- moduleTopLevels]
      usesIdentifiers = concat [getUseIdentifiers use | TopLevelUse use <- moduleTopLevels]

      baseScope = Map.fromList $ map (\ident -> (ident, ident)) (constantIdentifiers ++ usesIdentifiers)
   in currModule {moduleTopLevels = map (f [baseScope]) moduleTopLevels}
  where
    -- Then, proceed with unshadowing both the constants (right value only) and function declarations

    -- When encountering a function declaration:
    f scopes (TopLevelFunction fun@Function {functionParameters, functionBody = Just bodySequence}) =
      -- create a new local scope including the function parameters
      let functionParametersIdentifiers = map parameterIdentifier functionParameters
          scopes' = Map.fromList (map (\ident -> (ident, ident)) functionParametersIdentifiers) : scopes
       in -- remove shadowings in the function body
          TopLevelFunction $ fun {functionBody = Just $ removeShadowingInSequence bodySequence scopes'}
    -- When encountering a constant, inspect its right value
    f scopes (TopLevelConstant constant@Constant {constantExpression}) =
      TopLevelConstant $ constant {constantExpression = removeShadowingInExpr constantExpression scopes}
    --
    f _ other = other
