module Move.Translations.AlphaConversion (alphaConvert) where

import Data.Map qualified as Map
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traverseRootPostOrder)
import Move.Translations.Utils (Scope)

data AlphaState = AlphaState
  { alphaCounter :: Int,
    alphaScopes :: [Map.Map Identifier Identifier]
  }

-- | Renames colliding local bindings in every root and updates their references.
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

    freshIdentifier :: Identifier -> Int -> Map.Map Identifier Identifier -> (Identifier, Int)
    freshIdentifier (Identifier identifier) counter scope =
      let candidate = Identifier (identifier ++ "_" ++ show counter)
       in if Map.member candidate scope
            then freshIdentifier (Identifier identifier) (counter + 1) scope
            else (candidate, counter + 1)

    lookupIdentifier :: Identifier -> AlphaState -> Identifier
    lookupIdentifier identifier AlphaState {alphaScopes} =
      case lookupInScopes alphaScopes of
        Nothing -> identifier
        Just renamed -> renamed
      where
        lookupInScopes [] = Nothing
        lookupInScopes (scope : outerScopes) =
          case Map.lookup identifier scope of
            Just renamed -> Just renamed
            Nothing -> lookupInScopes outerScopes
