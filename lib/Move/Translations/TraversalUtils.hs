module Move.Translations.TraversalUtils (traverseExprPostOrder, traverseRootPostOrder, TraversalMapper, traversalIdentity) where

import Control.Monad.State qualified as State
import Data.Generics.Uniplate.Data (descendM)
import Data.List (find, mapAccumL)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Move.AST
import Move.Translations.Utils
  ( Scope,
    VariableAnnotations (VariableAnnotations),
    booleanType,
    extractVariablesFromBindings,
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
-- a custom state and all the other modules that might be imported. The function should return an expression to substitute and a new state
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
  let --
      -- First, add the uses to the local scope
      -- TODO: Currently not supported. It is better to extract all inner uses to top levels and just rename them to avoid name clashes,
      -- so to avoid having to supply external modules to the expression and sequence traversals
      usesScope = Map.empty
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
                bindScopes = Map.fromList $ map (\(ident, annot) -> (LocalNameAccessChain ident, annot)) $ extractVariablesFromBindings mappedBindings scopesBeforeBind
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
-- Additionally, allow to pass other modules that might be imported by the traversed one
traverseRootPostOrder :: TraversalMapper Expr state -> TraversalMapper Bindings state -> Root -> state -> [Root] -> (Root, state)
traverseRootPostOrder exprMapper bindsMapper root state otherRoots = case root of
  RModule rModule@Module {moduleTopLevels} ->
    let (mappedTopLevels, stateAfterTraversal) = traversalHelper moduleTopLevels
     in (RModule $ rModule {moduleTopLevels = mappedTopLevels}, stateAfterTraversal)
  RScript rScript@Script {scriptTopLevels} ->
    let (mappedTopLevels, stateAfterTraversal) = traversalHelper scriptTopLevels
     in (RScript $ rScript {scriptTopLevels = mappedTopLevels}, stateAfterTraversal)
  where
    -- Only modules can be imported, so discard any script
    otherModules = [m | RModule m <- otherRoots]

    traversalHelper topLevels =
      let --
          moduleUses :: [Use] = [u | TopLevelUse u <- topLevels]
          localScope = map (\(ident, annot) -> (LocalNameAccessChain ident, annot)) $ getRootIdentifiers root
          -- Build the scope of the current module, combining function and structs declarations, constants and imports
          moduleScope :: Scope = Map.fromList (localScope ++ buildImportedScope moduleUses otherModules)

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
                let functionParametersScope :: Scope = Map.fromList $ map (\(Parameter {parameterIdentifier, parameterType, parameterUUID}) -> (LocalNameAccessChain parameterIdentifier, VariableAnnotations parameterUUID parameterType)) functionParameters
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
    [ (LocalNameAccessChain $ Identifier "move_to", VariableAnnotations (Just $ -2) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") [], TypeConstructor (LocalNameAccessChain $ Identifier "T") [], IntermediateTypeWitnessType, unitType])),
      (LocalNameAccessChain $ Identifier "move_from", VariableAnnotations (Just $ -3) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, TypeConstructor (LocalNameAccessChain $ Identifier "T") []])),
      (LocalNameAccessChain $ Identifier "borrow_global_mut", VariableAnnotations (Just $ -4) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, TypeMutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "T") []])),
      (LocalNameAccessChain $ Identifier "borrow_global", VariableAnnotations (Just $ -5) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "T") []])),
      (LocalNameAccessChain $ Identifier "exists", VariableAnnotations (Just $ -5) (TypeArrow [Identifier "T"] [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeWitnessType, booleanType]))
    ]

-- |
-- Given multiple imports via Uses and the list of all the other available modules,
-- returns all the imported symbols
--
-- Note that the current module is not present in the imputs, just its imports
buildImportedScope :: [Use] -> [Module] -> [(NameAccessChain, VariableAnnotations)]
buildImportedScope uses otherModules = concatMap buildImportedScope' uses
  where
    buildImportedScope' :: Use -> [(NameAccessChain, VariableAnnotations)]
    buildImportedScope' use@(Use {useAddress, useIdentifier, useAlias, useMembers}) =
      let --
          -- First, retrieve the imported module, that is the module that matches both the address and identifier
          importedModule = find (\m -> moduleAddress m == useAddress && moduleIdentifier m == useIdentifier) otherModules

          -- If not found, throw an error
          -- FIXME: What about stdlib modules such as std:signer? it should be imported by default
          importedModule' = case importedModule of
            Nothing -> error $ "Imported module not found: " ++ show use
            Just m -> m

          -- Then, get all the symbols (identifiers declared in that module
          moduleDecls = getRootIdentifiers $ RModule importedModule'

          -- Each symbol can be used either by providing the full module nac, or the aliased one
          imported' :: [(NameAccessChain, VariableAnnotations)] =
            concatMap
              ( \(ident, annot) ->
                  [ (UnaliasedNameAccessChain useAddress useIdentifier ident, annot),
                    -- FIXME: this is a bit weird, since the module should be aliased by its identifier, not address
                    -- Check both the Parser.y with its comment on NameAccessChain and the Move compiler
                    -- Seems that the LeadingNameAccess allows for an identifier too
                    (AliasedNameAccessChain useAddress ident, annot)
                  ]
              )
              moduleDecls
       in -- If the module is aliased, symbols can also be used by the alias of that module
          -- FIXME: This is the same problem as the above fixme, basically the leading nac can be an identifier too
          -- imported'' = case useAlias of
          --   Nothing -> imported'
          --   Just useAlias' -> imported' ++ map (\(ident, annot) -> (AliasedNameAccessChain useAlias' ident, annot)) moduleDecls

          -- TODO: Use memebers, each with its own optional alias
          imported'

-- |
-- Given an AST (either script or module) returns all the identifiers defined as top levels of that AST,
--
-- This means declarations for named and positional structs, as well as function definitions and constant expressions,
-- while uses are instead not considered
getRootIdentifiers :: Root -> [(Identifier, VariableAnnotations)]
getRootIdentifiers root =
  let topLevels = case root of
        (RScript Script {scriptTopLevels}) -> scriptTopLevels
        (RModule Module {moduleTopLevels}) -> moduleTopLevels
   in concatMap topLevelMap topLevels
  where
    -- TODO: Positional structs should be handled similarly to named structs
    topLevelMap :: TopLevel -> [(Identifier, VariableAnnotations)]
    -- Uses are ignored in this function since they do not contribute the the local scope of the module
    topLevelMap (TopLevelUse _) = []
    topLevelMap (TopLevelFriend _) = []
    topLevelMap (TopLevelNamedStruct (NamedStruct {namedStructIdentifier, namedStructTypeParameters, namedStructFields})) = [(namedStructIdentifier, VariableAnnotations Nothing $ IntermediateTypeNamedStructDeclaration (map typeIdentifier namedStructTypeParameters) namedStructFields)]
    topLevelMap (TopLevelPositionalStruct (PositionalStruct {positionalStructIdentifier})) = [(positionalStructIdentifier, VariableAnnotations Nothing $ TypeConstructor (LocalNameAccessChain positionalStructIdentifier) [])]
    -- Functions have a type, which is the arrow type of all its parameter types and return type (or unit if not specified)
    -- Additionally, type parameters are considered
    topLevelMap (TopLevelFunction (Function {functionName, functionUUID, functionParameters, functionReturnType, functionTypeParameters})) =
      [(functionName, VariableAnnotations functionUUID $ TypeArrow (map typeIdentifier functionTypeParameters) (map parameterType functionParameters ++ [fromMaybe unitType functionReturnType]))]
    topLevelMap (TopLevelConstant (Constant {constantIdentifier, constantType, constantUUID})) = [(constantIdentifier, VariableAnnotations constantUUID constantType)]