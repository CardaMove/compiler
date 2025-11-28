module Move.Translations.Scopes where

import Data.Generics.Uniplate.Data (transformBi, universe)
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), getIdentifierFromScopes, inferExprType, unitType)

-- |
-- Given the AST, marks all the variables that need to be inserted in the explicit local scope
markVariablesForLocalScope :: Root -> Set.Set AnnotatedUUID
markVariablesForLocalScope root = snd $ traverseRootPostOrder exprMapper bindsMapper root Set.empty
  where
    insertMaybe :: (Ord a) => Maybe a -> Set.Set a -> Set.Set a
    insertMaybe Nothing set = set
    insertMaybe (Just val) set = Set.insert val set

    -- Given any expression, returns the UUID of the leftmost (local) identifier, if it is found
    getLeftmostIdentifier :: Expr -> [Scope] -> Maybe AnnotatedUUID
    getLeftmostIdentifier (NameAccessChainExpr (LocalNameAccessChain ident)) scopes =
      case getIdentifierFromScopes ident scopes of
        VariableAnnotations (Just identUUID) _ -> Just identUUID
        -- Throw an error if the identifier has no UUID
        _ -> error $ "Found identifier in right value without UUID when marking for local scope " ++ show ident
    getLeftmostIdentifier (DotOrIndexChainExpr DotAccess {dotAccessLeft}) scopes = getLeftmostIdentifier dotAccessLeft scopes
    getLeftmostIdentifier _ _ = Nothing

    exprMapper :: TraversalMapper Expr (Set.Set AnnotatedUUID)
    -- Every referenced variable `&[mut] a[.b.c]` should be added to the local scope
    exprMapper expr@(UnaryOpExpr (MutableReference referencedExpr)) scopes res = (expr, insertMaybe (getLeftmostIdentifier referencedExpr scopes) res)
    exprMapper expr@(UnaryOpExpr (ImmutableReference referencedExpr)) scopes res = (expr, insertMaybe (getLeftmostIdentifier referencedExpr scopes) res)
    -- Every mutated variable `a[.b.c] = ...` should be added to the local scope
    exprMapper expr@(AssignmentExpr (Assignment {assignmentLeft})) scopes res = (expr, insertMaybe (getLeftmostIdentifier assignmentLeft scopes) res)
    exprMapper expr _scopes res = (expr, res)

    bindsMapper :: TraversalMapper Bindings (Set.Set AnnotatedUUID)
    -- Regarding let bindings, there is no need to analyze anything
    -- In fact, reference variables do not always need to be inserted in the local scope
    -- They should be added only if mutated themselves, like normal variables
    bindsMapper binds _scopes mutRes = (binds, mutRes)

-- |
-- Utility function
--
-- Given an expression `a[.b.c]`, returns the chain of identifiers ["a", "b", "c"] in left-to-right order
getDotAccessChain :: Expr -> [Identifier]
getDotAccessChain = reverse . getDotAccessChain'
  where
    getDotAccessChain' (NameAccessChainExpr (LocalNameAccessChain ident)) = [ident]
    getDotAccessChain' (DotOrIndexChainExpr DotAccess {dotAccessLeft, dotAccessRight}) = dotAccessRight : getDotAccessChain' dotAccessLeft
    -- TODO: For now, only simple dot access chains like `a[.b.c]` are supported, but they might be inline values
    getDotAccessChain' expr = error $ "More complex dot access chain not supported: " ++ show expr

-- |
-- Utility function
--
-- Given a reference to either a local identifier or a dot access chain `&[mut] a[.b.c]`,
-- rewrites the node as an high level reference on the state, also preserving the resulting type
mapRightValueRef :: Expr -> Type -> Expr
mapRightValueRef (NameAccessChainExpr (LocalNameAccessChain ident)) identType = IntermediateExprExpr $ IntermediateReferenceLocalState [ident] identType
mapRightValueRef expr@(DotOrIndexChainExpr _) identType = IntermediateExprExpr $ IntermediateReferenceLocalState (getDotAccessChain expr) identType
-- TODO: Similarly, only references to variables or dot access are supported, but can be inline values
-- or also `(*a).b.c = ...`
mapRightValueRef expr _ = error $ "More complex reference not supported: " ++ show expr

-- |
-- Utility function
--
-- First pass: rewrite assignments (only left value) as let binding.
-- It uses a custom traversal so to allow converting an expression into a let binding inside the sequence
--    As a very picky comment, this will not match assignments that are inserted inside bigger expressions on right value,
--    such as `... (a = 1) ...`, but cases like this should never be present anyway
-- Possible assignment are `a[.b.c] = ...`, tuple destructuring `(a, b[.c]) = ...` or dereferences `*a`
rewriteAssignments :: Root -> Identifier -> AnnotatedUUID -> Root
rewriteAssignments root scopeIdentifier scopeUUID = transformBi f root
  where
    -- `a = ...`
    f (SequenceItemExpr (AssignmentExpr (Assignment {assignmentLeft = NameAccessChainExpr (LocalNameAccessChain ident), assignmentRight}))) =
      SequenceItemBindExpr
        Bindings
          { bindings = BindedSingle $ BindIdentifier scopeIdentifier $ Just scopeUUID,
            bindingsBindType = Just IntermediateTypeScopes,
            bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePutLocalState [ident] assignmentRight
          }
    -- `a[.b.c]`, note about syntactic sugar for references few comments below
    f (SequenceItemExpr (AssignmentExpr (Assignment {assignmentLeft = dotAccess@(DotOrIndexChainExpr _), assignmentRight}))) =
      SequenceItemBindExpr
        Bindings
          { bindings = BindedSingle $ BindIdentifier scopeIdentifier $ Just scopeUUID,
            bindingsBindType = Just IntermediateTypeScopes,
            bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePutLocalState (getDotAccessChain dotAccess) assignmentRight
          }
    -- `*a = ...`, it can not be a dot access since references can not be labels in structs
    f (SequenceItemExpr (AssignmentExpr (Assignment {assignmentLeft = UnaryOpExpr (Dereference (NameAccessChainExpr (LocalNameAccessChain ident))), assignmentRight}))) =
      SequenceItemBindExpr
        Bindings
          { bindings = BindedSingle $ BindIdentifier scopeIdentifier $ Just scopeUUID,
            bindingsBindType = Just IntermediateTypeScopes,
            bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePutDereferenceLocalState ident assignmentRight
          }
    -- TODO: tuple assignments
    f expr@(SequenceItemExpr _) = expr
    f bind = bind

-- |
-- Utility function
--
-- Second pass: rewrite references on right value as getters
-- (must be called after rewriting assignment to prevent updating derefs on left value)
rewriteRefs :: TraversalMapper Expr ()
rewriteRefs expr@(UnaryOpExpr (ImmutableReference referencedExpr)) scopes state = (mapRightValueRef referencedExpr $ inferExprType expr scopes, state)
rewriteRefs expr@(UnaryOpExpr (MutableReference referencedExpr)) scopes state = (mapRightValueRef referencedExpr $ inferExprType expr scopes, state)
-- Also rewrite dereferences on right value
-- This passage is mainly used to preserve the resulting type, that would not be inferrable anymore after the AST rewrite
rewriteRefs expr@(UnaryOpExpr (Dereference dereferencedExpr)) scopes state = (IntermediateExprExpr $ IntermediateGetDereferenceLocalState dereferencedExpr $ inferExprType expr scopes, state)
rewriteRefs expr _scopes state = (expr, state)

-- |
-- Utility function
--
-- Third pass: rewrite variables on right value, such as `a[.b.c]`
-- (must be done after references to avoid conflicts)
-- Note that the dot access chain is already handled with the leftmost identifier
-- This passage is mainly used to preserve the resulting type, so to allow for an explicit cast on the translated AST
rewriteVars :: Set.Set AnnotatedUUID -> TraversalMapper Expr ()
rewriteVars markedVars = rewriteVars'
  where
    rewriteVars' expr@(NameAccessChainExpr (LocalNameAccessChain ident)) scopes state =
      case getIdentifierFromScopes ident scopes of
        VariableAnnotations (Just identUUID) identType ->
          -- Replace the variable only if it is marked as such
          if Set.member identUUID markedVars
            then (IntermediateExprExpr $ IntermediateGetLocalState [ident] identType, state)
            else (expr, state)
        VariableAnnotations Nothing _ -> error $ "Found identifier without UUID when adding local state: " ++ show expr
    rewriteVars' expr _scopes state = (expr, state)

-- |
-- Utility function
--
-- Fourth pass: rewrite `let` bindings
-- (must be done as last so to preserve type inference)
-- only for variables that need to be inserted in the local scope
-- (note that the variable for the local state is not marked as such so will be automatically ignored)
rewriteLetBinds :: Set.Set AnnotatedUUID -> Identifier -> AnnotatedUUID -> TraversalMapper Bindings ()
rewriteLetBinds markedVars scopeIdentifier scopeUUID = rewriteLetBinds'
  where
    rewriteLetBinds' expr@(Bindings {bindings = BindedSingle (BindIdentifier ident identUUID), bindingsBindExpr}) _scopes state =
      case identUUID of
        Just identUUID' ->
          if Set.member identUUID' markedVars
            then
              ( Bindings
                  { bindings = BindedSingle $ BindIdentifier scopeIdentifier $ Just scopeUUID,
                    bindingsBindType = Just IntermediateTypeScopes,
                    bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePostLocalState ident bindingsBindExpr
                  },
                state
              )
            else (expr, state)
        Nothing -> error $ "Found let binding without UUID when adding local state: " ++ show expr
    -- TODO: Should also rewrite tuple bindings and destructuring of structs (since variables can still be updated)
    rewriteLetBinds' binds _scopes state = (binds, state)

-- |
-- Utility type for `rewriteInlineStateMutation`,
data FunctionReturns
  = NoRefs
  | ImmutableRefs
  | MutableRefs
  deriving (Eq, Ord)

-- |
-- Utility function
--
-- TODO: Also create and manage the variables for scopes: creation and return of outerscopes,
-- passing and retrieving scopes to functions that have references
-- or to sequences that have assignments (or references)
rewriteInlineStateMutation :: Identifier -> AnnotatedUUID -> TraversalMapper Expr (AnnotatedUUID, Map.Map Identifier Bindings)
rewriteInlineStateMutation scopeIdentifier scopeUUID = rewriteInlineStateMutation'
  where
    tempIdentifier = Identifier "temp_"

    -- TODO: For now considers just functions with a local name. To support calling functions in other module,
    -- the traversal should probably be rewritten so to support different modules and/or aliases and similar
    rewriteInlineStateMutation' expr@(PositionalStructExprOrFunctionCallExpr funcCall@PositionalStructExprOrFunctionCall {pseofcNameAccessChain = LocalNameAccessChain funcIdent, pseofcFields}) scopes (currUUID, bindingsToAdd) =
      case getIdentifierFromScopes funcIdent scopes of
        -- In this case, it is a function call and not a positional struct
        VariableAnnotations _ (TypeArrow typeArr) ->
          let refTypes = foldr checkParam NoRefs typeArr
                where
                  checkParam :: Type -> FunctionReturns -> FunctionReturns
                  checkParam (TypeMutableRef _) _ = MutableRefs
                  checkParam (TypeImmutableRef _) refTypes' = max ImmutableRefs refTypes'
                  checkParam _ refTypes' = refTypes'
           in case refTypes of
                -- The function does not accept or return references, leave as is
                NoRefs -> (expr, (currUUID, bindingsToAdd))
                -- The function accepts or returns immutable references. Append the state as last parameter
                ImmutableRefs ->
                  ( PositionalStructExprOrFunctionCallExpr
                      funcCall
                        { pseofcFields = pseofcFields ++ [NameAccessChainExpr $ LocalNameAccessChain scopeIdentifier]
                        },
                    (currUUID, bindingsToAdd)
                  )
                -- The function accepts or returns mutable references. It is needed to also return the updated state
                MutableRefs ->
                  -- create a new binding in the form `let (temp, scopes) = my_func(..., scopes)`
                  let -- The new variable should be `temp_i` to prevent name clashes with other rewrites
                      tempIdentifier' = case tempIdentifier of
                        Identifier str -> Identifier $ str ++ show currUUID

                      newBinding =
                        Bindings
                          { bindings =
                              BindedTuple
                                [ BindIdentifier tempIdentifier' (Just currUUID),
                                  BindIdentifier scopeIdentifier (Just scopeUUID)
                                ],
                            bindingsBindType =
                              Just $
                                TypeTuple
                                  [ last typeArr,
                                    IntermediateTypeScopes
                                  ],
                            bindingsBindExpr =
                              Just $
                                PositionalStructExprOrFunctionCallExpr
                                  funcCall
                                    { pseofcFields = pseofcFields ++ [NameAccessChainExpr $ LocalNameAccessChain scopeIdentifier]
                                    }
                          }
                   in -- and replace the function call with the temp variable
                      (NameAccessChainExpr $ LocalNameAccessChain tempIdentifier', (currUUID + 1, Map.insert tempIdentifier' newBinding bindingsToAdd))
        _ -> (expr, (currUUID, bindingsToAdd))
    --
    -- A sequence should be removed from the inline if it modifies the state, returning a new one
    -- This happens if any of it sequence items modify the state themselves.
    -- This can be known by looking for temporary variables inside
    -- probably there exist better ways to do it, but for now it is sufficient
    rewriteInlineStateMutation' (SequenceExpr Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) scopes (currUUID, bindingsToAdd) =
      let --
          -- Given an expression, return its temporary bindings,
          -- sorted by the order they have been created during the traversal
          -- The other element of the tuple is the updated Map of the temporary bindings
          -- In fact, temporary bindings already inspected should be removed in order to not be inserted multiple times,
          -- for example by an outer sequence that sees the temporary variables of an inner sequence
          getTemporaryBindingsOfExpr :: Expr -> Map.Map Identifier Bindings -> ([SequenceItem], Map.Map Identifier Bindings)
          getTemporaryBindingsOfExpr expr' bindingsToAdd''' =
            let -- Retrieve all identifiers in the expression
                identifiersInExpr = [ident | NameAccessChainExpr (LocalNameAccessChain ident) <- universe expr']
                -- Then, if the identifier has a temporary binding associated, retrieve it along with the UUID
                (identsUUIDWithBinding, bindingsToAdd'''') =
                  foldr
                    ( \ident (acc', bindingsToAdd''''') ->
                        case Map.updateLookupWithKey (\_k _val -> Nothing) ident bindingsToAdd''''' of
                          (Just binding, bindingsToAdd'''''') -> case getIdentifierFromScopes ident scopes of
                            VariableAnnotations (Just identUUID) _ -> ((identUUID, binding) : acc', bindingsToAdd'''''')
                            VariableAnnotations Nothing _ -> error $ "Found temporary variable without UUID: " ++ show ident
                          (Nothing, _) -> (acc', bindingsToAdd''''')
                    )
                    ([], bindingsToAdd''')
                    identifiersInExpr
                -- The UUID is used to sort the bindings, since it follows the post order visit
                -- and so the execution order
                sortedBinds = map (SequenceItemBindExpr . snd) $ sortOn fst identsUUIDWithBinding
             in (sortedBinds, bindingsToAdd'''')

          -- For each sequence item, prepend its temporary let bindings
          (sequenceItems', bindingsToAdd', mutatesState) = foldr handleSeqItem ([], bindingsToAdd, False) sequenceItems
            where
              hasIntermediateStateMutations :: Expr -> Bool
              hasIntermediateStateMutations expr' =
                any
                  ( \expr'' -> case expr'' of
                      IntermediateExprExpr (IntermediatePutLocalState _ _) -> True
                      IntermediateExprExpr (IntermediatePostLocalState _ _) -> True
                      IntermediateExprExpr (IntermediatePutDereferenceLocalState _ _) -> True
                      _ -> False
                  )
                  (universe expr')

              -- Given any sequence item, prepend its temporary bindings, the sequence item itself, and the accumulated items
              -- The other two elements of the tuple are the updated map of the temporary bindings
              -- and a boolean representing if the state is mutated in any way
              handleSeqItem :: SequenceItem -> ([SequenceItem], Map.Map Identifier Bindings, Bool) -> ([SequenceItem], Map.Map Identifier Bindings, Bool)
              handleSeqItem (SequenceItemExpr expr') (acc, bindingsToAdd''', mutatesState'') =
                case getTemporaryBindingsOfExpr expr' bindingsToAdd''' of
                  (sortedBinds, bindingsToAdd'''') -> (sortedBinds ++ SequenceItemExpr expr' : acc, bindingsToAdd'''', mutatesState'' || not (null sortedBinds) || hasIntermediateStateMutations expr')
              handleSeqItem (SequenceItemBindExpr bindings@Bindings {bindingsBindExpr = Just expr'}) (acc, bindingsToAdd''', mutatesState'') =
                case getTemporaryBindingsOfExpr expr' bindingsToAdd''' of
                  (sortedBinds, bindingsToAdd'''') -> (sortedBinds ++ SequenceItemBindExpr bindings : acc, bindingsToAdd'''', mutatesState'' || not (null sortedBinds) || hasIntermediateStateMutations expr')
              handleSeqItem (SequenceItemBindExpr bindings@Bindings {bindingsBindExpr = Nothing}) (acc, bindingsToAdd''', mutatesState'') = (SequenceItemBindExpr bindings : acc, bindingsToAdd''', mutatesState'')

          -- Also handle the ending expression
          (sequenceEndExpr', sequenceItems'', bindingsToAdd'', mutatesState') = case sequenceEndExpr of
            Just sequenceEndExpr'' -> case getTemporaryBindingsOfExpr sequenceEndExpr'' bindingsToAdd' of
              (sortedBinds, bindingsToAdd''') ->
                if mutatesState' || not (null sortedBinds)
                  -- If either the sequence items or the ending expression mutate state,
                  -- return the pair `(end_expr, tail scopes)` and append any temporary binding for the end expression to the sequence items
                  then
                    ( Just $ CommaExpr [sequenceEndExpr'', IntermediateExprExpr $ IntermediatePopScope scopeIdentifier],
                      sequenceItems' ++ sortedBinds,
                      bindingsToAdd''',
                      True
                    )
                  -- Otherwise just return the ending expression as is
                  else (Just sequenceEndExpr'', sequenceItems', bindingsToAdd''', False)
            Nothing ->
              if mutatesState
                -- If there is no ending expression but the state is still modified,
                -- return `((), tail scopes)`
                then
                  ( Just $ CommaExpr [CommaExpr [], IntermediateExprExpr $ IntermediatePopScope scopeIdentifier],
                    sequenceItems',
                    bindingsToAdd',
                    True
                  )
                -- Otherwise, do not return anything
                else (Nothing, sequenceItems', bindingsToAdd', False)

          -- In any case, append a new local state
          sequenceItems''' =
            ( SequenceItemBindExpr $
                Bindings
                  { bindings = BindedSingle $ BindIdentifier scopeIdentifier (Just scopeUUID),
                    bindingsBindType = Just IntermediateTypeScopes,
                    bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePushScope scopeIdentifier
                  }
            )
              : sequenceItems''

          -- Similar to the funciton invocation, the sequence is removed from the inline if it modifies the state
          (expr, (currUUID', bindingstoAdd''')) =
            if mutatesState'
              then
                let tempIdentifier' = case tempIdentifier of
                      Identifier str -> Identifier $ str ++ show currUUID

                    newBinding =
                      Bindings
                        { bindings =
                            BindedTuple
                              [ BindIdentifier tempIdentifier' (Just currUUID),
                                BindIdentifier scopeIdentifier (Just scopeUUID)
                              ],
                          bindingsBindType =
                            Just $
                              TypeTuple
                                [ maybe unitType (`inferExprType` scopes) sequenceEndExpr',
                                  IntermediateTypeScopes
                                ],
                          bindingsBindExpr =
                            Just $
                              SequenceExpr $
                                Sequence
                                  { sequenceUses = sequenceUses,
                                    sequenceItems = sequenceItems''',
                                    sequenceEndExpr = sequenceEndExpr'
                                  }
                        }
                 in ( NameAccessChainExpr $ LocalNameAccessChain tempIdentifier',
                      (currUUID + 1, Map.insert tempIdentifier' newBinding bindingsToAdd'')
                    )
              else
                ( SequenceExpr $
                    Sequence
                      { sequenceUses = sequenceUses,
                        sequenceItems = sequenceItems''',
                        sequenceEndExpr = sequenceEndExpr'
                      },
                  (currUUID, bindingsToAdd'')
                )
       in (expr, (currUUID', bindingstoAdd'''))
    -- TODO: Should also rewite if else. NOTE that is probably already modified wrongly (without considering conditional execution, return type instead is correct)
    -- during the universe function when examining the sequence
    -- A possible solution might be using an intermediate expression and handle it accordingly?
    rewriteInlineStateMutation' expr _scopes state = (expr, state)

-- |
-- Given an AST and the set of variables that need to be inserted into the local scope,
-- rewrites the usages of those variables with (intermediate) AST nodes that act on the explicit local scope
addLocalScopeInRoot :: Root -> Set.Set AnnotatedUUID -> AnnotatedUUID -> Root
addLocalScopeInRoot root markedVars currUUID =
  let scopeIdentifier = Identifier "scopes"
      scopeUUID :: AnnotatedUUID = -1

      -- TODO: NOTE: the dot chain can be used as a syntactic sugar for references, both on the left value and right value:
      -- `ref.a.b` is in fact identical to `(*ref).a.b`
      -- To support this, the runtime function to Get and Put LocalState need to check if the leftmost value is a reference,
      -- and if the access path is longer than one element, it means its actually a dot access so it mas meant to dereference the variabler

      firstPass :: Root = rewriteAssignments root scopeIdentifier scopeUUID
      secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
      thirdPass :: Root = fst $ traverseRootPostOrder (rewriteVars markedVars) traversalIdentity secondPass ()
      fourthPass :: Root = fst $ traverseRootPostOrder traversalIdentity (rewriteLetBinds markedVars scopeIdentifier scopeUUID) thirdPass ()
      fifthPass :: Root = fst $ traverseRootPostOrder (rewriteInlineStateMutation scopeIdentifier scopeUUID) traversalIdentity fourthPass (currUUID, Map.empty)
   in -- TODO: Should also rewrite function definitions similar to how function calls are handled

      -- TODO: Also note that right values might still have sequences inside, in any of the previous cases
      fifthPass