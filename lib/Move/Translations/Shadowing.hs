module Move.Translations.Shadowing (removeShadowingInRoot, removeShadowingInExpr, removeShadowingInSequence, getUnshadowedName, generateUnshadowedName) where

import Data.Generics.Uniplate.Data (Uniplate (descend))
import Data.List (mapAccumL)
import Data.Map qualified as Map
import Move.AST
import Move.Translations.Utils (getBindIdentifiers)

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
-- FIXME: Updates a bind name but does not update uuid
updateBindWithUnshadowedIdentifiersHelper :: BindedField -> [Scope] -> BindedField
updateBindWithUnshadowedIdentifiersHelper (BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing, bindedFieldUUID}) scopes = BindedField {bindFieldIdentifier = getUnshadowedName bindFieldIdentifier scopes, bindFieldInnerBind = Nothing, bindedFieldUUID}
updateBindWithUnshadowedIdentifiersHelper (BindedField {bindFieldIdentifier, bindFieldInnerBind = Just bindFieldInnerBind, bindedFieldUUID}) scopes = BindedField {bindFieldIdentifier, bindFieldInnerBind = Just $ updateBindWithUnshadowedIdentifiers bindFieldInnerBind scopes, bindedFieldUUID}

-- |
-- Given a single bind, replaces its binded identifiers with the newly unshadowed names
updateBindWithUnshadowedIdentifiers :: Bind -> [Scope] -> Bind
--    For a single identifier, replace it
updateBindWithUnshadowedIdentifiers (BindIdentifier ident uuid) scopes = BindIdentifier (getUnshadowedName ident scopes) uuid
--    For named structs, inspect its binded fields
updateBindWithUnshadowedIdentifiers (BindNamedStruct struct@BindedNamedStruct {bnsFields = fields@BindedFields {bindedFields}}) scopes =
  BindNamedStruct $ struct {bnsFields = fields {bindedFields = map (`updateBindWithUnshadowedIdentifiersHelper` scopes) bindedFields}}
--    Similar for positional structs
updateBindWithUnshadowedIdentifiers (BindPositionalStruct struct@BindedPositionalStruct {bpsFields = fields@BindedFields {bindedFields}}) scopes =
  BindPositionalStruct $ struct {bpsFields = fields {bindedFields = map (`updateBindWithUnshadowedIdentifiersHelper` scopes) bindedFields}}

-- |
-- Given an expression, removes any shadowing that happens in its scope and inner scopes
--
-- Must be invoked with at least one existing scope
removeShadowingInExpr :: Expr -> [Scope] -> Expr
--    When a local identifier is found, replace with its unshadowed name.
--    This is the base case for the recursion
--    Other access chains such as type names or struct/function names will not be matched by this pattern, since they are a `NameAccessChain`, not `NameAccessChainExpr`
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
-- Given a Sequence, adds a new scope and proceeds by renaming identifiers that cause shadowing,
--
-- while also collecting new identifiers as they are declared in the local scope.
--
-- Must be called with at least one scope. In fact, the topmost scope will be considered the local one.
--
-- This is useful when the Sequence is a function body, since the function parameters have to be on the local scope
--
-- Calls `removeShadowingInExpr` when it encounters an expression
removeShadowingInSequence :: Sequence -> [Scope] -> Sequence
removeShadowingInSequence (Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) scopes =
  let (scopes'', sequenceItems') = mapAccumL f scopes sequenceItems where
      f [] _ = error "Cannot have no scopes in Sequence"
      -- If the SequenceItem is an expression, recursively remove shadowing
      f scopes' (SequenceItemExpr expr) = (scopes', SequenceItemExpr $ removeShadowingInExpr expr scopes')
      -- If the SequenceItem is a let binding:
      f scopes'@(localScope : outerScopes) (SequenceItemBindExpr (Bindings {bindings, bindingsBindType, bindingsBindExpr})) = do
        -- First, retrieve all the identifiers that have been binded
        let newIdentifiers = case bindings of
              BindedSingle bind -> getBindIdentifiers bind
              BindedTuple binds -> concatMap getBindIdentifiers binds
        -- Since some of them can shadow other existing identifiers (declared on the outer scopes), generate new names for them
        -- Note that normal rebindings are permitted, so the local scope is not passed to `generateUnshadowedName`
        let unshadowedIdentifiers = map (\ident -> (ident, generateUnshadowedName ident outerScopes)) newIdentifiers
        -- Then create a map from them
        let newBindings = Map.fromList unshadowedIdentifiers

        ( -- The newly binded identifiers are added to the local scope
          Map.union newBindings localScope : outerScopes,
          -- The let binding itself is updated so to reflect the new names for the identifiers
          -- The right value of the bind is also recursively unshadowed, but without the newly introduced bindings
          SequenceItemBindExpr $
            Bindings
              { bindings =
                  case bindings of
                    BindedSingle bind -> BindedSingle $ updateBindWithUnshadowedIdentifiers bind [newBindings]
                    BindedTuple binds -> BindedTuple $ map (`updateBindWithUnshadowedIdentifiers` [newBindings]) binds,
                bindingsBindType,
                bindingsBindExpr = removeShadowingInExprMaybe bindingsBindExpr scopes'
              }
          )

      sequenceEndExpr' = removeShadowingInExprMaybe sequenceEndExpr scopes''
   in Sequence
        { sequenceUses,
          sequenceItems = sequenceItems',
          sequenceEndExpr = sequenceEndExpr'
        }

-- |
-- Given a Module or a script, first collects all identifiers belonging to constants declared in this module or script,
-- then proceeds by inspecting all function declarations to remove shadowings
-- TODO: Should also the "use" be considered in the scope?
removeShadowingInRoot :: Root -> Root
removeShadowingInRoot root = case root of
  RModule rModule@Module {moduleTopLevels} -> RModule $ rModule {moduleTopLevels = removeShadowingHelper moduleTopLevels}
  RScript rScript@Script {scriptTopLevels} -> RScript $ rScript {scriptTopLevels = removeShadowingHelper scriptTopLevels}
  where
    -- Proceed with unshadowing both the constants (right value only) and function declarations
    removeShadowingHelper topLevels = map (f [baseScope]) topLevels
      where
        -- All the constants declared in this module will act as a base scope for the module
        constantIdentifiers = [constantIdentifier | TopLevelConstant (Constant {constantIdentifier}) <- topLevels]
        baseScope = Map.fromList $ map (\ident -> (ident, ident)) constantIdentifiers

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
