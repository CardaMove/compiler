module Move.Translations.AlphaConversion (alphaConvert) where

import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traverseRootPostOrder)
import Move.Translations.Utils (Scope)

data AlphaState = AlphaState
  { alphaCounter :: Int,
    alphaScopes :: [Map.Map Identifier Identifier]
  }

-- |
-- Renames colliding local bindings in every root and updates their references.
-- Two bindings are colliding if they both reside in the same scope and have the same identifier.
-- In this case, the second is renamed by appending _i to its name, with i incremental counter (shared across the entire traversal).
-- All occurrences of that variable in right values are renamed accordingly.
-- Note that variables with no rebindings/clashes are left unmodified.
-- If two variables share the name identifier but reside on different scopes, no rename is performed.
-- Also note that local scope entries are left unmodified, since the CPS already handles them.
--
-- This step is performed so to support the lack of Javascript to rebind variables,
-- but it might also be inserted in the on-chain steps if a closer compilation between onchain and offchain needs to be mantained
alphaConvert :: [Root] -> [Root]
alphaConvert = convertRoots []
  where
    convertRoots :: [Root] -> [Root] -> [Root]
    convertRoots _ [] = []
    convertRoots previous (root : remaining) =
      convertRoot root (previous ++ remaining) : convertRoots (previous ++ [root]) remaining

    convertRoot :: Root -> [Root] -> Root
    convertRoot root otherRoots =
      fst $
        traverseRootPostOrder
          expressionMapper
          bindingMapper
          root
          (AlphaState 0 [])
          otherRoots

    expressionMapper :: TraversalMapper Expr AlphaState
    expressionMapper expression scopes state =
      let state' = synchronizeScopes scopes state
       in (rewriteExpression state' expression, state')

    bindingMapper :: TraversalMapper Bindings AlphaState
    bindingMapper bindings scopes state =
      let state' = synchronizeScopes scopes state
          (bindings', state'') = renameBindings bindings state'
       in (bindings', state'')

    -- The role of this function is to make sure the associations of original identifiers and renamed identifiers
    -- still respects the depth of the scope. Meaning that if a new scope is pushed, also the associations should include a new scope.
    -- This is done to avoid renaming variables with the name identifier but in different scopes
    -- Of course this function handles exiting from a scope by removing the no more needed associations
    synchronizeScopes :: [Scope] -> AlphaState -> AlphaState
    synchronizeScopes scopes state@AlphaState {alphaScopes} =
      let requiredDepth = length scopes
          currentDepth = length alphaScopes
       in if requiredDepth > currentDepth
            then state {alphaScopes = replicate (requiredDepth - currentDepth) Map.empty ++ alphaScopes}
            else state {alphaScopes = take requiredDepth alphaScopes}

    rewriteExpression :: AlphaState -> Expr -> Expr
    rewriteExpression state (NameAccessChainExpr (LocalNameAccessChain identifier)) =
      NameAccessChainExpr (LocalNameAccessChain (lookupIdentifier identifier state))
    rewriteExpression state (UnaryOpExpr (MoveExpr identifier)) =
      UnaryOpExpr (MoveExpr (lookupIdentifier identifier state))
    rewriteExpression state (UnaryOpExpr (CopyExpr identifier)) =
      UnaryOpExpr (CopyExpr (lookupIdentifier identifier state))
    rewriteExpression _ expression = expression

    renameBindings :: Bindings -> AlphaState -> (Bindings, AlphaState)
    renameBindings bindingGroup@Bindings {bindings = binded} state =
      let (binded', state') = renameBinded binded state
       in (bindingGroup {bindings = binded'}, state')

    renameBinded :: Binded -> AlphaState -> (Binded, AlphaState)
    renameBinded (BindedSingle bind) state =
      let (bind', state') = renameBind bind state
       in (BindedSingle bind', state')
    renameBinded (BindedTuple binds) state =
      let (renamed, state') = foldl renameOne ([], state) binds
       in (BindedTuple (reverse renamed), state')
      where
        renameOne (renamed, state') bind =
          let (bind', state'') = renameBind bind state'
           in (bind' : renamed, state'')

    renameBind :: Bind -> AlphaState -> (Bind, AlphaState)
    renameBind (BindIdentifier identifier annotation) state =
      let (identifier', state') = declare identifier state
       in (BindIdentifier identifier' annotation, state')
    renameBind (BindNamedStruct pattern') state =
      let (fields', state') = renameFields (bnsFields pattern') state
       in (BindNamedStruct pattern' {bnsFields = fields'}, state')
    renameBind (BindPositionalStruct pattern') state =
      let (fields', state') = renameFields (bpsFields pattern') state
       in (BindPositionalStruct pattern' {bpsFields = fields'}, state')

    renameFields :: BindedFields -> AlphaState -> (BindedFields, AlphaState)
    renameFields fields@BindedFields {bindedFields} state =
      let (renamed, state') = foldl renameOne ([], state) bindedFields
       in (fields {bindedFields = reverse renamed}, state')
      where
        renameOne (renamed, state') field =
          let (field', state'') = renameField field state'
           in (field' : renamed, state'')

    renameField :: BindedField -> AlphaState -> (BindedField, AlphaState)
    renameField field@BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing} state =
      let (identifier', state') = declare bindFieldIdentifier state
       in (field {bindFieldInnerBind = Just (BindIdentifier identifier' (bindedFieldUUID field))}, state')
    renameField field@BindedField {bindFieldInnerBind = Just innerBind} state =
      let (innerBind', state') = renameBind innerBind state
       in (field {bindFieldInnerBind = Just innerBind'}, state')

    declare :: Identifier -> AlphaState -> (Identifier, AlphaState)
    declare identifier state@AlphaState {alphaCounter, alphaScopes = currentScope : outerScopes} =
      if Map.notMember identifier currentScope
        then
          ( identifier,
            state {alphaScopes = Map.insert identifier identifier currentScope : outerScopes}
          )
        else
          let (candidate, nextCounter) = freshIdentifier identifier alphaCounter currentScope
              updatedScope = Map.insert identifier candidate (Map.insert candidate candidate currentScope)
           in (candidate, state {alphaCounter = nextCounter, alphaScopes = updatedScope : outerScopes})
    declare identifier state = (identifier, state)

    -- Generates a fresh variable identifier by appending _i to it when needed, with i incremental counter
    freshIdentifier :: Identifier -> Int -> Map.Map Identifier Identifier -> (Identifier, Int)
    freshIdentifier (Identifier identifier) counter scope =
      let candidate = Identifier (identifier ++ "_" ++ show counter)
       in if Map.member candidate scope
            then freshIdentifier (Identifier identifier) (counter + 1) scope
            else (candidate, counter + 1)

    -- Returns the new name associated with an identifier, or the same name if no rename has happened
    lookupIdentifier :: Identifier -> AlphaState -> Identifier
    lookupIdentifier identifier AlphaState {alphaScopes} =
      fromMaybe identifier (lookupInScopes alphaScopes)
      where
        lookupInScopes [] = Nothing
        lookupInScopes (scope : outerScopes) =
          case Map.lookup identifier scope of
            Just renamed -> Just renamed
            Nothing -> lookupInScopes outerScopes