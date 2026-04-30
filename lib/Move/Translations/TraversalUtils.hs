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
    aptosFrameworkLibAddress,
    booleanType,
    extractVariablesFromBindings,
    stdLibAddress,
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
    -- Additionally, add the modules provided by the Move stdlib
    otherModules = [m | RModule m <- otherRoots] ++ moveStdLibModules

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
-- such as global storage operators
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
-- Contains all the modules provided by the Move stdlib, such as the Signer module and the Coin module
-- These will be used simply to resolve imports and types, but will not generate any Aiken code
--
-- NOTE How the coin module is provided already with type witnesses and CPS, so to respect a fully translated module
moveStdLibModules :: [Module]
moveStdLibModules =
  [ Module
      { moduleAddress = stdLibAddress,
        moduleIdentifier = Identifier "signer",
        moduleTopLevels =
          [ TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "address_of",
                  functionTypeParameters = [],
                  functionParameters = [Parameter {parameterIdentifier = Identifier "signer", parameterType = TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") [], parameterUUID = Nothing}],
                  functionReturnType = Just $ TypeTuple [TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "borrow_address",
                  functionTypeParameters = [],
                  functionParameters = [Parameter {parameterIdentifier = Identifier "signer", parameterType = TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") [], parameterUUID = Nothing}],
                  functionReturnType = Just $ TypeTuple [TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [], IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                }
          ]
      },
    Module
      { moduleAddress = aptosFrameworkLibAddress,
        moduleIdentifier = Identifier "coin",
        moduleTopLevels =
          [ TopLevelNamedStruct $
              NamedStruct
                { namedStructIdentifier = Identifier "Coin",
                  namedStructTypeParameters = [TypeParameter {typeParameterIsPhantom = True, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  namedStructAbilities = [],
                  namedStructFields =
                    [ NamedField {fieldIdentifier = Identifier "value", fieldType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") []}
                    ]
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "withdraw",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "signer", parameterType = TypeImmutableRef $ TypeConstructor (LocalNameAccessChain $ Identifier "signer") [], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "amount", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "tw_coin_type", parameterType = IntermediateTypeWitnessType, parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "cps", parameterType = IntermediateTypeScopes, parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeTuple [TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "value",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "coin", parameterType = TypeImmutableRef $ TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "tw_coin_type", parameterType = IntermediateTypeWitnessType, parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "cps", parameterType = IntermediateTypeScopes, parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeTuple [TypeConstructor (LocalNameAccessChain $ Identifier "u64") [], IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "merge",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "dst_coin", parameterType = TypeMutableRef $ TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "source_coin", parameterType = TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "tw_coin_type", parameterType = IntermediateTypeWitnessType, parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "cps", parameterType = IntermediateTypeScopes, parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeTuple [unitType, IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "deposit",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "account_addr", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "address") [], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "coin", parameterType = TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "tw_coin_type", parameterType = IntermediateTypeWitnessType, parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "cps", parameterType = IntermediateTypeScopes, parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeTuple [unitType, IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "extract",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "coin", parameterType = TypeMutableRef $ TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "amount", parameterType = TypeConstructor (LocalNameAccessChain $ Identifier "u64") [], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "tw_coin_type", parameterType = IntermediateTypeWitnessType, parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "cps", parameterType = IntermediateTypeScopes, parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeTuple [TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                },
            TopLevelFunction $
              Function
                { functionHasNativeModifier = False,
                  functionVisibilityModifier = Just VisibilityModifierPublic,
                  functionHasEntryModifier = False,
                  functionName = Identifier "value",
                  functionTypeParameters = [TypeParameter {typeParameterIsPhantom = False, typeIdentifier = Identifier "CoinType", typeConstraints = []}],
                  functionParameters =
                    [ Parameter {parameterIdentifier = Identifier "coin", parameterType = TypeConstructor (AliasedNameAccessChain (Identifier "coin") (Identifier "Coin")) [TypeConstructor (LocalNameAccessChain $ Identifier "CoinType") []], parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "tw_coin_type", parameterType = IntermediateTypeWitnessType, parameterUUID = Nothing},
                      Parameter {parameterIdentifier = Identifier "cps", parameterType = IntermediateTypeScopes, parameterUUID = Nothing}
                    ],
                  functionReturnType = Just $ TypeTuple [TypeConstructor (LocalNameAccessChain $ Identifier "u64") [], IntermediateTypeScopes],
                  functionAcquires = [],
                  functionBody = Nothing,
                  functionUUID = Nothing
                }
          ]
      }
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
          importedModule' = case importedModule of
            Nothing -> error $ "Imported module not found: " ++ show use ++ ", total of other modules: " ++ show (length otherModules)
            Just m -> m

          -- Then, get all the symbols (identifiers declared in that module)
          moduleDecls = getRootIdentifiers $ RModule importedModule'

          -- Each symbol can be used either by providing the full module nac, or the aliased one
          imported' :: [(NameAccessChain, VariableAnnotations)] =
            concatMap
              ( \(ident, annot) ->
                  [ (UnaliasedNameAccessChain useAddress useIdentifier ident, annot),
                    (AliasedNameAccessChain useIdentifier ident, annot)
                  ]
              )
              moduleDecls
          -- If the module is aliased, symbols can also be used by the alias of that module
          imported'' = case useAlias of
            Nothing -> imported'
            Just useAlias' -> imported' ++ map (\(ident, annot) -> (AliasedNameAccessChain useAlias' ident, annot)) moduleDecls

          -- Imported members can also be used with their own local nac
          moduleDeclsByIdent = Map.fromList moduleDecls
          importedMembers :: [(NameAccessChain, VariableAnnotations)] = concatMap (`buildUseMember` moduleDeclsByIdent) useMembers
       in imported'' ++ importedMembers

    -- Given an imported member, along with all the modules declarations as map,
    -- returns the available definitions to refer to that member
    -- Note how the "Self" member is ignored, since it is already supported by this logic
    buildUseMember :: UseMember -> (Map.Map Identifier VariableAnnotations) -> [(NameAccessChain, VariableAnnotations)]
    buildUseMember UseMember {useMemberIdentifier = Identifier "Self"} _ = []
    buildUseMember UseMember {useMemberIdentifier, useMemberUseAlias} declsByIdent =
      let annot = case Map.lookup useMemberIdentifier declsByIdent of
            Nothing -> error $ "Non existing useMember: " ++ show useMemberIdentifier
            Just annot' -> annot'
          -- If the member is aliased, this is another way to refer to it
          aliased = case useMemberUseAlias of
            Nothing -> []
            Just alias -> [(LocalNameAccessChain alias, annot)]
       in (LocalNameAccessChain useMemberIdentifier, annot) : aliased

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