module Move.Translations.TraversalUtils (traverseExprPostOrder, traverseModulePostOrder) where

import Control.Monad.State qualified as State
import Data.Generics.Uniplate.Data (descendM)
import Data.List (mapAccumL)
import Data.Map qualified as Map
import Move.AST
import Move.Translations.Utils

type TraverseExprMapper state = Expr -> [Scope] -> state -> (Expr, state)

-- |
-- Performs a post-order traversal of a tree of expressions.
--
-- Accepts as input a function that takes the current expression, all the accumulated scopes at that node,
-- and a custom state. The function should return an expression to substitute and a new state
traverseExprPostOrder :: TraverseExprMapper state -> Expr -> [Scope] -> state -> (Expr, state)
--      When a sequence is found, call the specific traversal for it and then invoke f on the resulting Sequence
--      Note that outside of a SequenceExpr, the scope is not modified
traverseExprPostOrder f (SequenceExpr sequence') scopes state =
  let (sequence'', state') = traverseSequencePostOrder f sequence' (Map.empty : scopes) state
   in f (SequenceExpr sequence'') scopes state'
--      When any other expression is found, recursively descend with `descendM` to keep the state
traverseExprPostOrder f expr scopes state =
  let (expr''', state''') = State.runState (descendM fDesc expr) state
        where
          fDesc expr' = do
            state' <- State.get
            let (expr'', state'') = traverseExprPostOrder f expr' scopes state'
            State.put state''
            return expr''
   in f expr''' scopes state'''

-- |
-- Similar to `traverseExprPostOrder` but restricted to a Sequence

-- The topmost scope is considered the local one, so at least one must be provided
traverseSequencePostOrder :: TraverseExprMapper state -> Sequence -> [Scope] -> state -> (Sequence, state)
traverseSequencePostOrder _ _ [] _ = error "Cannot traverse Sequence with no scopes"
traverseSequencePostOrder f (Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) (localScope : outerScopes) state =
  -- First, add the uses to the local scope
  let usesIdentifiers = concatMap getUseIdentifiers sequenceUses
      usesScope = Map.fromList (map (,Nothing) usesIdentifiers)
      scopesBeforeSeqItems = Map.union usesScope localScope : outerScopes
      -- Then, recursively traverse the sequence items, and collect the new scope and state
      ((scopesAfterSeqItems, stateAfterSeqItems), sequenceItems') = mapAccumL fAcc (scopesBeforeSeqItems, state) sequenceItems
        where
          fAcc ([], _) _ = error "Cannot traverse SequenceItem with no scopes"
          -- When an expression is encountered, traverse it
          fAcc (scopesBeforeExpr, stateBeforeExpr) (SequenceItemExpr expr) =
            let (expr', stateAfterTraversingExpr) = traverseExprPostOrder f expr scopesBeforeExpr stateBeforeExpr
             in -- It can be noticed that traversing an expression has not changed the scope
                ((scopesBeforeExpr, stateAfterTraversingExpr), SequenceItemExpr expr')
          -- When a binding is encountered in the sequence:
          fAcc (scopesBeforeBind@(localScopeBeforeBind : outerScopesBeforeBind), stateBeforeBind) (SequenceItemBindExpr bindings@(Bindings {bindingsBindExpr})) =
            -- first, traverse the binded expression and retrieve the new expression and state
            let (traversedBindExpr, stateAfterTraversingBindExpr) = case bindingsBindExpr of
                  Nothing -> (Nothing, stateBeforeBind)
                  Just bindExpr ->
                    let (mappedBindExpr, state') = traverseExprPostOrder f bindExpr scopesBeforeBind stateBeforeBind
                     in (Just mappedBindExpr, state')
                -- Note: here should be called any function that would map the binding
                -- Then, add the binded identifiers to the local scope
                bindScopes = Map.fromList $ inferBindingsTypes bindings
                localScopeAfterBind = Map.union bindScopes localScopeBeforeBind
             in ( (localScopeAfterBind : outerScopesBeforeBind, stateAfterTraversingBindExpr),
                  SequenceItemBindExpr $
                    bindings {bindingsBindExpr = traversedBindExpr}
                )

      -- Then, traverse the ending expression
      (sequenceEndExpr', stateAfterEndExpr) = case sequenceEndExpr of
        Nothing -> (Nothing, stateAfterSeqItems)
        Just endExpr ->
          let (mappedEndExpr, state') = traverseExprPostOrder f endExpr scopesAfterSeqItems stateAfterSeqItems
           in (Just mappedEndExpr, state')
   in -- Finally, return the Sequence with updated expressions and a new state
      ( Sequence
          { sequenceUses,
            sequenceItems = sequenceItems',
            sequenceEndExpr = sequenceEndExpr'
          },
        stateAfterEndExpr
      )

-- |
-- Performs a post-order traversal of an entire module.
--
-- Invokes a function for each encountered expression (see `traverseExprPostOrder`), and returns the resulting Module and state
traverseModulePostOrder :: (Expr -> [Scope] -> state -> (Expr, state)) -> Module -> state -> (Module, state)
traverseModulePostOrder f currModule@Module {moduleTopLevels} state =
  -- Since top level identifiers are never renamed, add all of them to the module scope before starting to traverse
  let identifiersAndType = concatMap topLevelMap moduleTopLevels
        where
          topLevelMap (TopLevelUse use) = map (,Nothing) (getUseIdentifiers use)
          topLevelMap (TopLevelFriend _) = []
          -- Structs and function declarations for now do not have a type
          topLevelMap (TopLevelNamedStruct (NamedStruct {namedStructIdentifier})) = [(namedStructIdentifier, Nothing)]
          topLevelMap (TopLevelPositionalStruct (PositionalStruct {positionalStructIdentifier})) = [(positionalStructIdentifier, Nothing)]
          topLevelMap (TopLevelFunction (Function {functionName})) = [(functionName, Nothing)]
          topLevelMap (TopLevelConstant (Constant {constantIdentifier, constantType})) = [(constantIdentifier, Just constantType)]
      moduleScope = Map.fromList identifiersAndType

      (stateAfterStraversal, mappedTopLevels) = mapAccumL topLevelMap state moduleTopLevels
        where
          -- Traverse the constant expressions
          -- Note that the scope is unchanged, only the state is forwarded
          topLevelMap state' (TopLevelConstant constant@Constant {constantExpression}) =
            let (mappedExpr, state'') = traverseExprPostOrder f constantExpression [moduleScope] state'
             in (state'', TopLevelConstant $ constant {constantExpression = mappedExpr})
          -- Traverse the function declarations that have a body.
          -- Each function will have an additional scope for both the parameters and the body
          topLevelMap state' (TopLevelFunction function@Function {functionParameters, functionBody = Just bodySequence}) =
            -- Create a new local scope including the function parameters
            let functionParametersScope = Map.fromList $ map (\(Parameter{parameterIdentifier, parameterType}) -> (parameterIdentifier, Just parameterType)) functionParameters
                (mappedSequence, state'') = traverseSequencePostOrder f bodySequence [functionParametersScope, moduleScope] state'
             in (state'', TopLevelFunction $ function {functionBody = Just mappedSequence})
          -- Otherwise do nothing
          topLevelMap state' topLevel = (state', topLevel)
   in (currModule {moduleTopLevels = mappedTopLevels}, stateAfterStraversal)