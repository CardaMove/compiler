module Move.Translations.TraversalUtils (traverseExprPostOrder, traverseRootPostOrder, TraversalMapper, traversalIdentity) where

import Control.Monad.State qualified as State
import Data.Generics.Uniplate.Data (descendM)
import Data.List (mapAccumL)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Move.AST
import Move.Translations.Utils
  ( Scope,
    VariableAnnotations (VariableAnnotations),
    booleanType,
    extractVariablesFromBindings,
    getUseIdentifiers,
    unitType,
  )

type TraversalMapper node state = node -> [Scope] -> state -> (node, state)

-- |
-- Used to traverse either an expression or a `let` bind without producing any effect,
-- acting as a sort of identity for both the node and the state
traversalIdentity :: TraversalMapper node state
traversalIdentity node _ state = (node, state)

-- |
-- Performs a post-order traversal of a tree of expressions.
--
-- Accepts as input a function that takes the current expression, all the accumulated scopes at that node,
-- and a custom state. The function should return an expression to substitute and a new state
--
-- As its second version, includes another function to map `let` bindings found in inner sequences
traverseExprPostOrder :: TraversalMapper Expr state -> TraversalMapper Bindings state -> Expr -> [Scope] -> state -> (Expr, state)
--      When a sequence is found, call the specific traversal for it and then invoke f on the resulting Sequence
--      Note that outside of a SequenceExpr, the scope is not modified
traverseExprPostOrder exprMapper bindsMapper (SequenceExpr sequence') scopes state =
  let (sequence'', state') = traverseSequencePostOrder exprMapper bindsMapper sequence' (Map.empty : scopes) state
   in exprMapper (SequenceExpr sequence'') scopes state'
--      When any other expression is found, recursively descend with `descendM` to keep the state
traverseExprPostOrder exprMapper bindsMapper expr scopes state =
  let (expr''', state''') = State.runState (descendM fDesc expr) state
        where
          fDesc expr' = do
            state' <- State.get
            let (expr'', state'') = traverseExprPostOrder exprMapper bindsMapper expr' scopes state'
            State.put state''
            return expr''
   in exprMapper expr''' scopes state'''

-- |
-- Similar to `traverseExprPostOrder` but restricted to a Sequence

-- The topmost scope is considered the local one, so at least one must be provided
traverseSequencePostOrder :: TraversalMapper Expr state -> TraversalMapper Bindings state -> Sequence -> [Scope] -> state -> (Sequence, state)
traverseSequencePostOrder _ _ _ [] _ = error "Cannot traverse Sequence with no scopes"
traverseSequencePostOrder exprMapper bindsMapper (Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) (localScope : outerScopes) state =
  -- First, add the uses to the local scope
  -- TODO: They will not have any UUID nor type
  let usesIdentifiers = concatMap getUseIdentifiers sequenceUses
      usesScope :: Scope = Map.fromList (map (,VariableAnnotations Nothing TypeUnknown) usesIdentifiers)
      scopesBeforeSeqItems = Map.union usesScope localScope : outerScopes
      -- Then, recursively traverse the sequence items, and collect the new scope and state
      ((scopesAfterSeqItems, stateAfterSeqItems), sequenceItems') = mapAccumL fAcc (scopesBeforeSeqItems, state) sequenceItems
        where
          fAcc ([], _) _ = error "Cannot traverse SequenceItem with no scopes"
          -- When an expression is encountered, traverse it
          fAcc (scopesBeforeExpr, stateBeforeExpr) (SequenceItemExpr expr) =
            let (expr', stateAfterTraversingExpr) = traverseExprPostOrder exprMapper bindsMapper expr scopesBeforeExpr stateBeforeExpr
             in -- It can be noticed that traversing an expression has not changed the scope
                ((scopesBeforeExpr, stateAfterTraversingExpr), SequenceItemExpr expr')
          -- When a binding is encountered in the sequence:
          fAcc (scopesBeforeBind@(localScopeBeforeBind : outerScopesBeforeBind), stateBeforeBind) (SequenceItemBindExpr bindings@(Bindings {bindingsBindExpr})) =
            -- first, traverse the binded expression and retrieve the new expression and state
            let (traversedBindExpr, stateAfterTraversingBindExpr) = case bindingsBindExpr of
                  Nothing -> (Nothing, stateBeforeBind)
                  Just bindExpr ->
                    let (mappedBindExpr, state') = traverseExprPostOrder exprMapper bindsMapper bindExpr scopesBeforeBind stateBeforeBind
                     in (Just mappedBindExpr, state')
                -- Then, call the mapped function for the bindings, passing the scope before this binding,
                -- but the state after traversing the right value
                -- Also note that it is passed the mapped right value, not the original one
                (mappedBindings, stateAfterTraversingBindings) = bindsMapper bindings {bindingsBindExpr = traversedBindExpr} scopesBeforeBind stateAfterTraversingBindExpr
                -- Then, add the binded identifiers to the local scope
                bindScopes = Map.fromList $ extractVariablesFromBindings mappedBindings scopesBeforeBind
                localScopeAfterBind = Map.union bindScopes localScopeBeforeBind
             in ( (localScopeAfterBind : outerScopesBeforeBind, stateAfterTraversingBindings),
                  SequenceItemBindExpr mappedBindings
                )

      -- Then, traverse the ending expression
      (sequenceEndExpr', stateAfterEndExpr) = case sequenceEndExpr of
        Nothing -> (Nothing, stateAfterSeqItems)
        Just endExpr ->
          let (mappedEndExpr, state') = traverseExprPostOrder exprMapper bindsMapper endExpr scopesAfterSeqItems stateAfterSeqItems
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
-- Performs a post-order traversal of an entire module or script.
--
-- Invokes a function for each encountered expression (see `traverseExprPostOrder`), and returns the resulting Module and state
--
-- As its second version, also includes a function to map `let` bindings
--
-- TODO: Should probably be rewritten so to pass scopes coming from other modules
traverseRootPostOrder :: TraversalMapper Expr state -> TraversalMapper Bindings state -> Root -> state -> (Root, state)
traverseRootPostOrder exprMapper bindsMapper root state = case root of
  RModule rModule@Module {moduleTopLevels} ->
    let (mappedTopLevels, stateAfterTraversal) = traversalHelper moduleTopLevels
     in (RModule $ rModule {moduleTopLevels = mappedTopLevels}, stateAfterTraversal)
  RScript rScript@Script {scriptTopLevels} ->
    let (mappedTopLevels, stateAfterTraversal) = traversalHelper scriptTopLevels
     in (RScript $ rScript {scriptTopLevels = mappedTopLevels}, stateAfterTraversal)
  where
    traversalHelper topLevels =
      -- Since top level identifiers are never renamed, add all of them to the module scope before starting to traverse
      let topLevelIdentifiersAnnotated = concatMap topLevelMap topLevels
            where
              -- TODO: uses for now do not have a type
              -- TODO: Positional structs should be handled similarly to named structs
              topLevelMap (TopLevelUse use) = map (,VariableAnnotations Nothing TypeUnknown) (getUseIdentifiers use)
              topLevelMap (TopLevelFriend _) = []
              topLevelMap (TopLevelNamedStruct (NamedStruct {namedStructIdentifier, namedStructTypeParameters, namedStructFields})) = [(namedStructIdentifier, VariableAnnotations Nothing $ IntermediateTypeNamedStructDeclaration (map typeIdentifier namedStructTypeParameters) namedStructFields)]
              topLevelMap (TopLevelPositionalStruct (PositionalStruct {positionalStructIdentifier})) = [(positionalStructIdentifier, VariableAnnotations Nothing $ TypeConstructor (LocalNameAccessChain positionalStructIdentifier) [])]
              -- Functions have a type, which is the arrow type of all its parameter types and return type (or unit if not specified)
              -- Additionally, type parameters are considered
              topLevelMap (TopLevelFunction (Function {functionName, functionUUID, functionParameters, functionReturnType, functionTypeParameters})) =
                [(functionName, VariableAnnotations functionUUID $ TypeArrow (map typeIdentifier functionTypeParameters) (map parameterType functionParameters ++ [fromMaybe unitType functionReturnType]))]
              topLevelMap (TopLevelConstant (Constant {constantIdentifier, constantType, constantUUID})) = [(constantIdentifier, VariableAnnotations constantUUID constantType)]
          moduleScope :: Scope = Map.fromList topLevelIdentifiersAnnotated

          -- Add the std lib to the scope
          topLevelScope :: [Scope] = [moduleScope, moveStdLibScope]

          (stateAfterTraversal, mappedTopLevels) = mapAccumL topLevelMap state topLevels
            where
              -- Traverse the constant expressions
              -- Note that the scope is unchanged, only the state is forwarded
              topLevelMap state' (TopLevelConstant constant@Constant {constantExpression}) =
                let (mappedExpr, state'') = traverseExprPostOrder exprMapper bindsMapper constantExpression topLevelScope state'
                 in (state'', TopLevelConstant $ constant {constantExpression = mappedExpr})
              -- Traverse the function declarations that have a body.
              -- Each function will have an additional scope for both the parameters and the body
              topLevelMap state' (TopLevelFunction function@Function {functionParameters, functionBody = Just bodySequence}) =
                -- Create a new local scope including the function parameters
                let functionParametersScope :: Scope = Map.fromList $ map (\(Parameter {parameterIdentifier, parameterType, parameterUUID}) -> (parameterIdentifier, VariableAnnotations parameterUUID parameterType)) functionParameters
                    (mappedSequence, state'') = traverseSequencePostOrder exprMapper bindsMapper bodySequence (functionParametersScope : topLevelScope) state'
                 in (state'', TopLevelFunction $ function {functionBody = Just mappedSequence})
              -- Otherwise do nothing
              topLevelMap state' topLevel = (state', topLevel)
       in (mappedTopLevels, stateAfterTraversal)

-- |
-- Contains some functions that Move recognizes as stdlib,
-- such as global storage operators and TODO: Coin library
--
-- Note that the global storage definitions have a final parameter for the type witness.
-- This will correspond to the actual AST only after the TypeWitness step,
-- meaning that before it, the actual function invocation is missing that argument
moveStdLibScope :: Scope
moveStdLibScope =
  Map.fromList
    [ (Identifier "move_to", VariableAnnotations (Just $ -2) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") [], TypeConstructor (LocalNameAccessChain $ Identifier "T") [], IntermediateTypeWitnessType, unitType])),
      (Identifier "move_from", VariableAnnotations (Just $ -3) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, TypeConstructor (LocalNameAccessChain $ Identifier "T") []])),
      (Identifier "borrow_global_mut", VariableAnnotations (Just $ -4) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, TypeMutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "T") []])),
      (Identifier "borrow_global", VariableAnnotations (Just $ -5) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "T") []])),
      (Identifier "exists", VariableAnnotations (Just $ -5) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, booleanType]))
    ]