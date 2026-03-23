module Move.Translations.Scopes (markVariablesForLocalScope, rewriteAssignments, rewriteRefs, rewriteVars, rewriteLetBinds, rewriteInlineStateMutation, addLocalScopeInRoot) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Generics.Uniplate.Data (transformBi, universe)
import Data.List (elemIndex, sortOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), cpsIdentifier, getNacFromScopes, inferExprType, iterateWithOthers, unitType)

-- |
-- Given the AST, marks all the variables that need to be inserted in the explicit local scope
--
-- Accepts other modules that might be imported by the current one, but the motivation is simply for better inferring expression types,
-- since in any case it is not expected for non-local variables to be mutated in any way and thus inserted into the scope
markVariablesForLocalScope :: Root -> [Root] -> Set.Set AnnotatedUUID
markVariablesForLocalScope root otherRoots = snd $ traverseRootPostOrder vaiableMarker bindsMapper root Set.empty otherRoots
  where
    insertMaybe :: (Ord a) => Maybe a -> Set.Set a -> Set.Set a
    insertMaybe Nothing set = set
    insertMaybe (Just val) set = Set.insert val set

    -- Given any expression, returns the UUID of the leftmost (local) identifier, if it is found
    getLeftmostIdentInRef :: Expr -> [Scope] -> Maybe AnnotatedUUID
    getLeftmostIdentInRef (NameAccessChainExpr (LocalNameAccessChain ident)) scopes =
      case getNacFromScopes (LocalNameAccessChain ident) scopes of
        VariableAnnotations (Just identUUID) _ -> Just identUUID
        -- Throw an error if the identifier has no UUID
        _ -> error $ "Found identifier in right value without UUID when marking for local scope " ++ show ident
    getLeftmostIdentInRef (DotOrIndexChainExpr DotAccess {dotAccessLeft}) scopes = getLeftmostIdentInRef dotAccessLeft scopes
    -- References to non-local name access chains would make sense only for immutable references to module constants,
    -- but the Move compiler seems to complain in any case
    getLeftmostIdentInRef nac@(NameAccessChainExpr _) _ = error $ "Unsupported reference to non-local name access chain: " ++ show nac
    getLeftmostIdentInRef _ _ = Nothing

    vaiableMarker :: TraversalMapper Expr (Set.Set AnnotatedUUID)
    -- Every referenced variable `&[mut] a[.b.c]` should be added to the local scope
    vaiableMarker expr@(UnaryOpExpr (MutableReference referencedExpr)) scopes res = (expr, insertMaybe (getLeftmostIdentInRef referencedExpr scopes) res)
    vaiableMarker expr@(UnaryOpExpr (ImmutableReference referencedExpr)) scopes res = (expr, insertMaybe (getLeftmostIdentInRef referencedExpr scopes) res)
    -- Every mutated variable `a[.b.c] = ...` should be added to the local scope
    vaiableMarker expr@(AssignmentExpr (Assignment {assignmentLeft})) scopes res = (expr, insertMaybe (getLeftmostIdentInRef assignmentLeft scopes) res)
    vaiableMarker expr _scopes res = (expr, res)

    bindsMapper :: TraversalMapper Bindings (Set.Set AnnotatedUUID)
    -- Regarding let bindings, there is no need to analyze anything
    -- In fact, reference variables do not always need to be inserted in the local scope
    -- They should be added only if mutated themselves, like normal variables
    bindsMapper binds _scopes mutRes = (binds, mutRes)

-- |
-- Utility function
--
-- Given all the temporary bindings, retrieves the original type of the temporary identifiers as a new Scope
-- In this way, it is possible to infer the type of expressions that depend on those temporary identifiers
--
-- FIXME: This is called multiple times with the same input, and moreover might often be not needed
-- Can a possible fix be inserting this scope as the last one, and leave the work to the lazy evaluation?
mapTemporaryBindingsToScope :: Map.Map Identifier Bindings -> Scope
mapTemporaryBindingsToScope bindingsToAdd = Map.map handleTempBind $ Map.mapKeys LocalNameAccessChain bindingsToAdd
  where
    handleTempBind :: Bindings -> VariableAnnotations
    handleTempBind
      Bindings
        { bindings = BindedTuple [BindIdentifier _ tempUUID, _],
          bindingsBindType = Just (TypeTuple [tempT, IntermediateTypeScopes])
        } = VariableAnnotations tempUUID tempT
    handleTempBind bind = error $ "Unexpected temporary binding: " ++ show bind

-- |
-- Utility function
--
-- Given an expression `a[.b.c]`, returns (ordered):
--
-- - The leftmost identifier `a`
--
-- - The chain of fields' indices `[idx_b, idx_c]` in left-to-right order
--
-- - The type of the identifier `a`

--
-- Instead, the final type is not returned so to not duplicate the logic of `inferExprType`
--
-- Needs the scope so to find the correct structs.
--
-- Note that `a` must be a local name
unconstrDotAccess :: Expr -> [Scope] -> (Identifier, [Int], Type)
unconstrDotAccess expr' scopes' =
  let (ident, idx, tIdent) = unconstrDotAccess' expr' scopes'
   in (ident, reverse idx, tIdent)
  where
    -- Helper function. Returns the field indices in reverse order for convenience
    unconstrDotAccess' :: Expr -> [Scope] -> (Identifier, [Int], Type)
    -- Leftmost part of a dot access should be a local identifier
    unconstrDotAccess' expr@(NameAccessChainExpr (LocalNameAccessChain ident)) scopes = (ident, [], inferExprType expr scopes)
    unconstrDotAccess' expr@(NameAccessChainExpr _) _ = error $ "Unexpected dot access to non-local name access chain: " ++ show expr
    -- When finding a dot access, recursively check the left part
    unconstrDotAccess' expr@(DotOrIndexChainExpr (DotAccess {dotAccessLeft, dotAccessRight})) scopes =
      case (unconstrDotAccess' dotAccessLeft scopes, inferExprType dotAccessLeft scopes) of
        -- The left part produces a value of a type that might also be a reference, due to syntactic sugar
        -- Here the syntactic sugar is ignored, it is handled outside this function
        -- Note: in all cases, always append the recursive field indices and also return leftmost identifier name/type already found in the recursion
        ((identLeft, fieldsChain, tLeft), TypeConstructor tCons _) ->
          let fieldIdx = accessField tCons dotAccessRight
           in (identLeft, fieldIdx : fieldsChain, tLeft)
        ((identLeft, fieldsChain, tLeft), TypeMutableRef (TypeConstructor tCons _)) ->
          let fieldIdx = accessField tCons dotAccessRight
           in (identLeft, fieldIdx : fieldsChain, tLeft)
        ((identLeft, fieldsChain, tLeft), TypeImmutableRef (TypeConstructor tCons _)) ->
          let fieldIdx = accessField tCons dotAccessRight
           in (identLeft, fieldIdx : fieldsChain, tLeft)
        res -> error $ "Dot access to a non-struct type: " ++ show expr ++ " as: " ++ show res
      where
        -- Given the name of type constructor of the left part of the dot access,
        -- along with the name of the field to access, accesses the field from the struct, returning
        -- the index of the field
        -- Note that the resulting type is ignored. If needed, it can be inferred by using `inferExprType`
        accessField :: NameAccessChain -> Identifier -> Int
        -- The type of the left part of the dot access must be a (local) named struct
        accessField (LocalNameAccessChain tCons) fieldIdent =
          case getNacFromScopes (LocalNameAccessChain tCons) scopes of
            VariableAnnotations _ str@(IntermediateTypeNamedStructDeclaration _ namedFields) ->
              -- If so, access the correct field
              case elemIndex fieldIdent $ map fieldIdentifier namedFields of
                Nothing -> error $ "Dot access to unknown struct field: " ++ show str ++ ", field: " ++ show fieldIdent
                Just fieldIdx -> fieldIdx
            VariableAnnotations _ t -> error $ "Dot access to a non-named struct: " ++ show expr ++ " as: " ++ show t
        -- Otherwise, throw and error, since it is not possible to access struct fields from other modules
        accessField tCons fieldIdent = error $ "Unexpected non-local type when unconstructing dot access: " ++ show (tCons, fieldIdent)

    -- TODO: For now, only simple dot access chains like `a[.b.c]` are supported, but they might be inline values
    unconstrDotAccess' expr _ = error $ "Unsupported dot access chain: " ++ show expr

-- |
-- Utility function
--
-- First pass: rewrite assignments (only left value) as let binding.
-- It uses a custom traversal so to allow converting an expression into a let binding inside the sequence
--    As a very picky comment, this will not match assignments that are inserted inside bigger expressions on right value,
--    such as `... (a = 1) ...`, but cases like this should never be present anyway
--
-- Possible assignment are `a[.b.c] = ...`, tuple destructuring `(a, b[.c]) = ...` or dereferences `*a`
--
-- The rewriting is performed on two steps:
-- first, a traversal uses the scope to rewrite the assignments into an `IntermediateScopeBinding` expression, that must be present only in this function
-- then, a transformBi is performed in order to rewrite a `SequenceItemExpr` corresponding to the assignement into a let bind `SequenceItemBindExpr`
rewriteAssignments :: Root -> [Root] -> AnnotatedUUID -> Root
rewriteAssignments root otherRoots scopeUUID = transformBi helperBindRewriter $ fst $ traverseRootPostOrder exprMapper traversalIdentity root () otherRoots
  where
    -- Translates assignments into the `IntermediateScopeBinding` expressions
    exprMapper :: TraversalMapper Expr ()
    -- Translating `a = ...` as a PUT on the state
    exprMapper (AssignmentExpr (Assignment {assignmentLeft = NameAccessChainExpr (LocalNameAccessChain ident), assignmentRight})) _ state =
      (IntermediateExprExpr $ IntermediateScopeBinding [IntermediatePutLocalState ident assignmentRight []], state)
    -- Translating `a.b[.c] = ...`.
    -- Note that the it might be a syntactic sugar if `a` is a reference
    exprMapper (AssignmentExpr (Assignment {assignmentLeft = dotAccess@(DotOrIndexChainExpr _), assignmentRight})) scopes state =
      case unconstrDotAccess dotAccess scopes of
        -- In case `a` is a reference, and some fields are accessed, is a syntactic sugar
        (ident, fields@(_ : _), TypeMutableRef _) -> (IntermediateExprExpr $ IntermediateScopeBinding [IntermediatePutDereferenceLocalState ident assignmentRight fields], state)
        (ident, fields@(_ : _), TypeImmutableRef _) -> (IntermediateExprExpr $ IntermediateScopeBinding [IntermediatePutDereferenceLocalState ident assignmentRight fields], state)
        -- Otherwise it is a normal PUT on the state
        (ident, fields, _) -> (IntermediateExprExpr $ IntermediateScopeBinding [IntermediatePutLocalState ident assignmentRight fields], state)
    -- Translating `*a = ...`, with no dot access
    -- Note that `a` must be a local name
    exprMapper (AssignmentExpr (Assignment {assignmentLeft = UnaryOpExpr (Dereference (NameAccessChainExpr (LocalNameAccessChain ident))), assignmentRight})) _ state =
      (IntermediateExprExpr $ IntermediateScopeBinding [IntermediatePutDereferenceLocalState ident assignmentRight []], state)
    -- TODO: Tuple assignments
    exprMapper expr _ state = (expr, state)

    -- rewrites a `IntermediateScopeBinding` into a let bind `SequenceItemBindExpr`
    helperBindRewriter :: SequenceItem -> SequenceItem
    -- TODO: Support for tuple assignments by subsequent bindings in nested struct (or if possible, multiple lines of let bindings)
    helperBindRewriter (SequenceItemExpr (IntermediateExprExpr (IntermediateScopeBinding [rightVal]))) =
      SequenceItemBindExpr
        Bindings
          { bindings = BindedSingle $ BindIdentifier cpsIdentifier $ Just scopeUUID,
            bindingsBindType = Just IntermediateTypeScopes,
            bindingsBindExpr = Just $ IntermediateExprExpr rightVal
          }
    helperBindRewriter bind = bind

-- |
-- Utility function
--
-- Second pass: rewrite references and dereferences on right value as getters
-- (must be called after rewriting assignment to prevent updating derefs on left value)
--
-- Here, it is assumed that references are only in the form of `& a[.b.c]`, so a dot access or a name access chain is expected
-- Additionally, if `a` is already a reference, then it should be performed an extension of that reference
--
-- Also includes global storage operators that do not alter the storage itself, meaning borrows and exists
rewriteRefs :: TraversalMapper Expr ()
rewriteRefs (UnaryOpExpr (ImmutableReference expr)) scopes state =
  case (unconstrDotAccess expr scopes, inferExprType expr scopes) of
    -- In case the leftmost identifier is a reference, it is a reference extension
    ((identLeft, fieldIdxs, TypeMutableRef _), tFinal) -> (IntermediateExprExpr $ IntermediateReferenceExtension (NameAccessChainExpr (LocalNameAccessChain identLeft)) fieldIdxs (TypeImmutableRef tFinal), state)
    ((identLeft, fieldIdxs, TypeImmutableRef _), tFinal) -> (IntermediateExprExpr $ IntermediateReferenceExtension (NameAccessChainExpr (LocalNameAccessChain identLeft)) fieldIdxs (TypeImmutableRef tFinal), state)
    -- Otherwise, it is a normal reference
    ((identLeft, fieldIdxs, _), tFinal) -> (IntermediateExprExpr $ IntermediateReferenceLocalState identLeft fieldIdxs $ TypeImmutableRef tFinal, state)
rewriteRefs (UnaryOpExpr (MutableReference expr)) scopes state =
  case (unconstrDotAccess expr scopes, inferExprType expr scopes) of
    -- In case the leftmost identifier is a reference, it is a reference extension
    ((identLeft, fieldIdxs, TypeMutableRef _), tFinal) -> (IntermediateExprExpr $ IntermediateReferenceExtension (NameAccessChainExpr (LocalNameAccessChain identLeft)) fieldIdxs (TypeMutableRef tFinal), state)
    -- Note that it is not possible to mutare an already immutable reference
    ((_, _, TypeImmutableRef _), _) -> error $ "Trying to extend an immutable reference into a mutable reference: " ++ show expr
    -- Otherwise, it is a normal reference
    ((identLeft, fieldIdxs, _), tFinal) -> (IntermediateExprExpr $ IntermediateReferenceLocalState identLeft fieldIdxs $ TypeMutableRef tFinal, state)
-- Also rewrite dereferences on right value
-- This passage is mainly used to preserve the resulting type, that would not be inferrable anymore after the AST rewrite
rewriteRefs expr@(UnaryOpExpr (Dereference dereferencedExpr)) scopes state = (IntermediateExprExpr $ IntermediateGetDereferenceLocalState dereferencedExpr $ inferExprType expr scopes, state)
-- TODO: another traversal -- On the right value, there exists a syntactic sugar for `r.b[.c]` where if `r` is already a reference, it acts as `(*r).b[.c]`
-- -- Note how there is no need to check if `b` is a reference, since refs can not be stored as struct fields
-- rewriteRefs expr@(DotOrIndexChainExpr (DotAccess{dotAccessLeft, dotAccessRight})) scopes state =
--   let (nac, idx, t) = unconstrDotAccess expr scopes
--    in case getNacFromScopes nac scopes of
--      VariableAnnotations _ (TypeImmutableRef _) ->

-- Also rewrite `borrow_global_mut` and `borrow_global`, treating them as normal references but on the global storage
rewriteRefs
  expr@( PositionalStructExprOrFunctionCallExpr
           ( PositionalStructExprOrFunctionCall
               { pseofcNameAccessChain = LocalNameAccessChain (Identifier "borrow_global_mut"),
                 pseofcTypeArgs,
                 pseofcFields = [addressExpr, IntermediateExprExpr (IntermediateTypeWitnessExprExpr typeWitnessExpr)]
               }
             )
         )
  _scopes
  state = case pseofcTypeArgs of
    [] -> error $ "Found a borrow_global_mut with invalid type arguments: " ++ show expr
    [t] -> (IntermediateExprExpr $ IntermediateBorrowGlobalMut addressExpr t typeWitnessExpr, state)
    _ -> error $ "Found a borrow_global_mut with invalid type arguments: " ++ show expr
rewriteRefs
  expr@( PositionalStructExprOrFunctionCallExpr
           ( PositionalStructExprOrFunctionCall
               { pseofcNameAccessChain = LocalNameAccessChain (Identifier "borrow_global"),
                 pseofcTypeArgs,
                 pseofcFields = [addressExpr, IntermediateExprExpr (IntermediateTypeWitnessExprExpr typeWitnessExpr)]
               }
             )
         )
  _scopes
  state = case pseofcTypeArgs of
    [] -> error $ "Found a borrow_global with invalid type arguments: " ++ show expr
    [t] -> (IntermediateExprExpr $ IntermediateBorrowGlobal addressExpr t typeWitnessExpr, state)
    _ -> error $ "Found a borrow_global with invalid type arguments: " ++ show expr
-- Also rewrite the `exists<T>(address)`
rewriteRefs
  expr@( PositionalStructExprOrFunctionCallExpr
           ( PositionalStructExprOrFunctionCall
               { pseofcNameAccessChain = LocalNameAccessChain (Identifier "exists"),
                 pseofcTypeArgs,
                 pseofcFields = [addressExpr, IntermediateExprExpr (IntermediateTypeWitnessExprExpr typeWitnessExpr)]
               }
             )
         )
  _scopes
  state = case pseofcTypeArgs of
    [] -> error $ "Found a exists with invalid type arguments: " ++ show expr
    [t] -> (IntermediateExprExpr $ IntermediateExists addressExpr t typeWitnessExpr, state)
    _ -> error $ "Found a exists with invalid type arguments: " ++ show expr
rewriteRefs expr _scopes state = (expr, state)

-- |
-- Utility function
--
-- Third pass: rewrite variables on right value, such as `a[.b.c]`
-- (must be done after references to avoid conflicts)
--
-- Additionally, performs desugaring when encountering a dot access `r.a[.b]` where `r` is a reference
-- This is because, if encountered, it is equivalent to `(*r).a[.b]`
-- Note that this does not cause conflits with reference extension `& r.a[.b]` since it has already been rewritten as a different AST node
--
-- This passage is mainly used to preserve the resulting type, so to allow for an explicit cast on the translated AST
rewriteVars :: Set.Set AnnotatedUUID -> TraversalMapper Expr ()
rewriteVars markedVars = rewriteVars'
  where
    -- Handle any local identifier, including the leftmost identifier in any dot access chain
    rewriteVars' expr@(NameAccessChainExpr (LocalNameAccessChain ident)) scopes state =
      case getNacFromScopes (LocalNameAccessChain ident) scopes of
        VariableAnnotations (Just identUUID) identType ->
          -- Replace the variable only if it is marked as such
          if Set.member identUUID markedVars
            then (IntermediateExprExpr $ IntermediateGetLocalState ident identType, state)
            else (expr, state)
        VariableAnnotations Nothing _ -> error $ "Found identifier without UUID when adding local state: " ++ show expr
    rewriteVars' expr@(DotOrIndexChainExpr DotAccess {dotAccessLeft, dotAccessRight}) scopes state =
      case inferExprType dotAccessLeft scopes of
        -- If the left part of the dot access is a reference, wrap it into a dereference
        -- Note that this happens only for the leftmost dot access, meaning `r.a` and not `r.a.b`, since the inference of the expression
        -- defaults to syntact sugar for dot accesses, so it would not return a reference for the type of `r.a` in the second case
        -- Fot the same reason, do not include the reference type here for the left part
        TypeMutableRef tLeft ->
          ( DotOrIndexChainExpr $
              DotAccess
                { dotAccessLeft = IntermediateExprExpr $ IntermediateGetDereferenceLocalState dotAccessLeft tLeft,
                  dotAccessRight
                },
            state
          )
        TypeImmutableRef tLeft ->
          ( DotOrIndexChainExpr $
              DotAccess
                { dotAccessLeft = IntermediateExprExpr $ IntermediateGetDereferenceLocalState dotAccessLeft tLeft,
                  dotAccessRight
                },
            state
          )
        _ -> (expr, state)
    rewriteVars' expr _scopes state = (expr, state)

-- |
-- Utility function
--
-- Fourth pass: rewrite `let` bindings
-- (must be done as last so to preserve type inference)
-- only for variables that need to be inserted in the local scope
-- (note that the variable for the local state is not marked as such so will be automatically ignored)
rewriteLetBinds :: Set.Set AnnotatedUUID -> AnnotatedUUID -> TraversalMapper Bindings ()
rewriteLetBinds markedVars scopeUUID = rewriteLetBinds'
  where
    rewriteLetBinds' expr@(Bindings {bindings = BindedSingle (BindIdentifier ident identUUID), bindingsBindExpr}) _scopes state =
      case identUUID of
        Just identUUID' ->
          if Set.member identUUID' markedVars
            then
              ( Bindings
                  { bindings = BindedSingle $ BindIdentifier cpsIdentifier $ Just scopeUUID,
                    bindingsBindType = Just IntermediateTypeScopes,
                    bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePostLocalState ident bindingsBindExpr
                  },
                state
              )
            else (expr, state)
        Nothing -> error $ "Found let binding without UUID when adding local state: " ++ show expr
    -- TODO: Should also rewrite tuple bindings and destructuring of structs (since variables can still be updated)
    -- Probably it can be done with custom traversal to return multiple lines
    rewriteLetBinds' binds _scopes state = (binds, state)

-- |
-- Utility function
--
-- It is a traversal that rewrites each function invocation (including `move_to` and `move_from`) and sequence (that modifies the state) into a let binding and the corresponding temporary variable.
--
-- For example, suppose `my_func(&mut a)`, it is rewritten as `let (temp_i, scopes) = my_func(&mut a, scopes)` and replaced by `temp_i`.
--
-- This is always performed on function calls, since they might mutate the global storage and it is not possible to know in advance (at least without a proper static analysis).
--
-- Similar for a sequence: `let (temp_i, scopes) = {...}`
rewriteInlineStateMutation :: AnnotatedUUID -> TraversalMapper Expr (AnnotatedUUID, Map.Map Identifier Bindings)
rewriteInlineStateMutation scopeUUID = rewriteInlineStateMutation'
  where
    tempIdentifier = Identifier "temp_"

    rewriteInlineStateMutation' :: TraversalMapper Expr (AnnotatedUUID, Map.Map Identifier Bindings)
    -- Rewrite `move_to<T>(&signer, resource)` into a let binding.
    --
    -- This will result in a temporary variable inserted in its place, that for the most cases will be useless,
    -- however in the (unlikely) case where a `move_to` is present inside an expression, will make it work by returning a unit
    --
    -- This works similarly to how any function call is rewritten
    rewriteInlineStateMutation'
      expr@( PositionalStructExprOrFunctionCallExpr
               PositionalStructExprOrFunctionCall
                 { pseofcNameAccessChain = LocalNameAccessChain (Identifier "move_to"),
                   pseofcTypeArgs,
                   pseofcFields = [signerExpr, resourceExpr, IntermediateExprExpr (IntermediateTypeWitnessExprExpr typeWitnessExpr)]
                 }
             )
      scopes
      (currUUID, bindingsToAdd) =
        let --
            -- To correctly infer the type, it might be needed to access some temporary variables,
            -- so they have been added as a new scope.
            -- Adding a scope on top does not cause issues in this step
            resourceType = case pseofcTypeArgs of
              [] -> inferExprType resourceExpr (scopes ++ [mapTemporaryBindingsToScope bindingsToAdd])
              [t] -> t
              _ -> error $ "Found a move_to with invalid type arguments: " ++ show expr

            -- create a new binding in the form `let (temp, scopes) = my_func(..., scopes)`
            -- The new variable should be `temp_i` to prevent name clashes with other rewrites
            -- temp_i will always have unit value `()`
            tempIdentifier' = case tempIdentifier of
              Identifier str -> Identifier $ str ++ show currUUID

            newBinding =
              Bindings
                { bindings =
                    BindedTuple
                      [ BindIdentifier tempIdentifier' (Just currUUID),
                        BindIdentifier cpsIdentifier (Just scopeUUID)
                      ],
                  bindingsBindType =
                    Just $
                      TypeTuple
                        [ unitType,
                          IntermediateTypeScopes
                        ],
                  bindingsBindExpr =
                    Just $ IntermediateExprExpr $ IntermediateMoveTo signerExpr resourceExpr resourceType typeWitnessExpr
                }
         in -- and replace the function call with the temp variable
            (NameAccessChainExpr $ LocalNameAccessChain tempIdentifier', (currUUID + 1, Map.insert tempIdentifier' newBinding bindingsToAdd))
    -- Handle the `move_from<T>(address)` similarly to the `move_to` case
    rewriteInlineStateMutation'
      expr@( PositionalStructExprOrFunctionCallExpr
               PositionalStructExprOrFunctionCall
                 { pseofcNameAccessChain = LocalNameAccessChain (Identifier "move_from"),
                   pseofcTypeArgs,
                   pseofcFields = [addressExpr, IntermediateExprExpr (IntermediateTypeWitnessExprExpr typeWitnessExpr)]
                 }
             )
      _scopes
      (currUUID, bindingsToAdd) =
        let --
            -- The type argument can not be omitted and must be a single one
            resourceType = case pseofcTypeArgs of
              [] -> error $ "Found a move_from with invalid type arguments: " ++ show expr
              [t] -> t
              _ -> error $ "Found a move_from with invalid type arguments: " ++ show expr

            -- create a new binding in the form `let (temp, scopes) = my_func(..., scopes)`
            -- The new variable should be `temp_i` to prevent name clashes with other rewrites
            tempIdentifier' = case tempIdentifier of
              Identifier str -> Identifier $ str ++ show currUUID

            newBinding =
              Bindings
                { bindings =
                    BindedTuple
                      [ BindIdentifier tempIdentifier' (Just currUUID),
                        BindIdentifier cpsIdentifier (Just scopeUUID)
                      ],
                  bindingsBindType =
                    Just $
                      TypeTuple
                        [ resourceType,
                          IntermediateTypeScopes
                        ],
                  bindingsBindExpr =
                    Just $ IntermediateExprExpr $ IntermediateMoveFrom addressExpr resourceType typeWitnessExpr
                }
         in -- and replace the function call with the temp variable
            (NameAccessChainExpr $ LocalNameAccessChain tempIdentifier', (currUUID + 1, Map.insert tempIdentifier' newBinding bindingsToAdd))
    rewriteInlineStateMutation' expr@(PositionalStructExprOrFunctionCallExpr funcCall@PositionalStructExprOrFunctionCall {pseofcNameAccessChain, pseofcFields}) scopes (currUUID, bindingsToAdd) =
      case getNacFromScopes pseofcNameAccessChain scopes of
        -- In this case, it is a function call and not a positional struct
        VariableAnnotations _ (TypeArrow _ _) ->
          -- Since it is not possible (without a proper static analysis) to know if the function call accesses or modify the state,
          -- for now assume it always does that
          --
          let -- create a new binding in the form `let (temp, scopes) = my_func(..., scopes)`
              -- The new variable should be `temp_i` to prevent name clashes with other rewrites
              tempIdentifier' = case tempIdentifier of
                Identifier str -> Identifier $ str ++ show currUUID

              newBinding =
                Bindings
                  { bindings =
                      BindedTuple
                        [ BindIdentifier tempIdentifier' (Just currUUID),
                          BindIdentifier cpsIdentifier (Just scopeUUID)
                        ],
                    bindingsBindType =
                      Just $
                        TypeTuple
                          [ -- To correctly infer the type, it might be needed to access some temporary variables,
                            -- so they have been added as a new scope.
                            -- Adding a scope on top does not cause issues in this step
                            -- FIXME: Maybe it is needed to check if the function already updates the state, before creating a tuple?
                            -- similar to the bug dolved for inline sequences in `rewriteInlineStateMutation'`
                            inferExprType expr (scopes ++ [mapTemporaryBindingsToScope bindingsToAdd]),
                            IntermediateTypeScopes
                          ],
                    bindingsBindExpr =
                      Just $
                        PositionalStructExprOrFunctionCallExpr
                          funcCall
                            { pseofcFields = pseofcFields ++ [NameAccessChainExpr $ LocalNameAccessChain cpsIdentifier]
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
    rewriteInlineStateMutation' (SequenceExpr seqnce) scopes (currUUID, bindingsToAdd) =
      let --
          -- Rewrite the sequence
          (seq', bindingsToAdd', mutatesState) = rewriteInlineStateMutationInSequence seqnce scopeUUID bindingsToAdd True False

          -- Similar to the function invocation, the sequence is removed from the inline if it modifies the state
          (expr, (currUUID', bindingstoAdd'')) =
            if mutatesState
              then
                let tempIdentifier' = case tempIdentifier of
                      Identifier str -> Identifier $ str ++ show currUUID

                    newBinding =
                      Bindings
                        { bindings =
                            BindedTuple
                              [ BindIdentifier tempIdentifier' (Just currUUID),
                                BindIdentifier cpsIdentifier (Just scopeUUID)
                              ],
                          -- To correctly infer the type, it might be needed to access some temporary variables,
                          -- so they have been added as a new scope.
                          -- Adding a scope on top does not cause issues in this step
                          bindingsBindType =
                            Just $ inferExprType (SequenceExpr seq') (scopes ++ [mapTemporaryBindingsToScope bindingsToAdd]),
                          bindingsBindExpr = Just $ SequenceExpr seq'
                        }
                 in ( NameAccessChainExpr $ LocalNameAccessChain tempIdentifier',
                      (currUUID + 1, Map.insert tempIdentifier' newBinding bindingsToAdd)
                    )
              else
                ( SequenceExpr seq',
                  (currUUID, bindingsToAdd')
                )
       in (expr, (currUUID', bindingstoAdd''))
    -- In case of if-then-else, each branch can be an Expr (SequenceExpr included, if if does not modify the state)
    -- Then, if the branch has some temporary bindings in it, it means it modifies the state.
    -- In this case, the whole branch is replaced by a sequence, complete with PUSH/POP of a new scope, with all the temporary bindings in it.
    -- By doing so, it is respected that a mutation will not happen if is not in the branch that is actually executed
    -- Additionally, the branch will return `(end_expr, tail scopes)`, meaning that the other branch needs to be aligned,
    -- and also that the if-then-else as a whole needs to be replaced by a temporary variable
    rewriteInlineStateMutation' (IfThenElseTerm expr@IfThenElse {ifThenElseIfBranch, ifThenElseElseBranch}) scopes (currUUID, bindingsToAdd) =
      let (tempBindsOfIfBranch, bindingsToAdd') = getTemporaryBindingsOfExpr ifThenElseIfBranch bindingsToAdd
          (tempBindsOfElseBranch, bindingsToAdd'') = case ifThenElseElseBranch of
            Nothing -> ([], bindingsToAdd')
            Just ifThenElseElseBranch' -> getTemporaryBindingsOfExpr ifThenElseElseBranch' bindingsToAdd'

          -- Given either the if or the else branch, with the corresponding temporary bindings,
          -- returns a SequenceExpr that contains all the temporary bindings and always returns `(end_expr, tail scopes)`,
          -- even if the branch does not modify the state. This is so to align the return types of the branches
          rewriteBranch :: Expr -> [SequenceItem] -> Expr
          rewriteBranch branch tempBindings =
            SequenceExpr $
              Sequence
                { sequenceUses = [],
                  sequenceItems =
                    -- Push a new scope
                    SequenceItemBindExpr
                      ( Bindings
                          { bindings = BindedSingle $ BindIdentifier cpsIdentifier (Just scopeUUID),
                            bindingsBindType = Just IntermediateTypeScopes,
                            bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePushScope cpsIdentifier
                          }
                      )
                      -- Add the temporary bindings
                      : tempBindings,
                  -- Return the pair `(end_expr, tail scopes)`
                  sequenceEndExpr = Just $ CommaExpr [branch, IntermediateExprExpr $ IntermediatePopScope cpsIdentifier]
                }
       in case (tempBindsOfIfBranch, tempBindsOfElseBranch) of
            -- In this case, both branches are pure
            ([], []) -> (IfThenElseTerm expr, (currUUID, bindingsToAdd''))
            -- Otherwise, both branches need to be rewritten to return `(end_expr, tail scopes)`,
            -- and the whole if-then-else needs to be replaced by a temporary variable
            _ ->
              let -- create a new binding in the form `let (temp, scopes) = if-then-else`
                  -- The new variable should be `temp_i` to prevent name clashes with other rewrites
                  tempIdentifier' = case tempIdentifier of
                    Identifier str -> Identifier $ str ++ show currUUID

                  newBinding =
                    Bindings
                      { bindings =
                          BindedTuple
                            [ BindIdentifier tempIdentifier' (Just currUUID),
                              BindIdentifier cpsIdentifier (Just scopeUUID)
                            ],
                        bindingsBindType =
                          Just $
                            TypeTuple
                              [ -- To correctly infer the type, it might be needed to access some temporary variables,
                                -- so they have been added as a new scope.
                                -- Adding a scope on top does not cause issues in this step
                                inferExprType (IfThenElseTerm expr) (scopes ++ [mapTemporaryBindingsToScope bindingsToAdd]),
                                IntermediateTypeScopes
                              ],
                        -- As the binded expression, use the if-then-else with the rewritten branches
                        bindingsBindExpr =
                          Just $
                            IfThenElseTerm
                              expr
                                { ifThenElseIfBranch = rewriteBranch ifThenElseIfBranch tempBindsOfIfBranch,
                                  -- Regarding the else branch, in case it is not present, add a dummy branch consisting in a sequence that simply returns `(unit, scopes)`
                                  ifThenElseElseBranch = case ifThenElseElseBranch of
                                    Nothing ->
                                      Just $
                                        SequenceExpr $
                                          Sequence
                                            { sequenceUses = [],
                                              sequenceItems = [],
                                              sequenceEndExpr = Just $ CommaExpr [CommaExpr [], NameAccessChainExpr $ LocalNameAccessChain cpsIdentifier]
                                            }
                                    Just ifThenElseElseBranch' -> Just $ rewriteBranch ifThenElseElseBranch' tempBindsOfElseBranch
                                }
                      }
               in -- and replace the if-then-else itself with the temporary variable
                  (NameAccessChainExpr $ LocalNameAccessChain tempIdentifier', (currUUID + 1, Map.insert tempIdentifier' newBinding bindingsToAdd))
    rewriteInlineStateMutation' expr _scopes state = (expr, state)

-- |
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
              -- Here, retrieve the temporary binding associated to this identifier (if exists)
              case Map.updateLookupWithKey (\_k _val -> Nothing) ident bindingsToAdd''''' of
                -- If exists, it must be in the form `let (temp_i, scopes) = ...`, so extract the UUID of temp_i
                (Just binding@Bindings {bindings = BindedTuple [BindIdentifier _ identUUID, _]}, bindingsToAdd'''''') -> case identUUID of
                  (Just identUUID') -> ((identUUID', binding) : acc', bindingsToAdd'''''')
                  Nothing -> error $ "Found temporary variable without UUID: " ++ show ident
                (Just binding, _) -> error $ "Found unexpected temporary binding associated to UUID " ++ show ident ++ ": " ++ show binding
                (Nothing, _) -> (acc', bindingsToAdd''''')
          )
          ([], bindingsToAdd''')
          identifiersInExpr
      -- The UUID is used to sort the bindings, since it follows the post order visit
      -- and so the execution order
      sortedBinds = map (SequenceItemBindExpr . snd) $ sortOn fst identsUUIDWithBinding
   in (sortedBinds, bindingsToAdd'''')

-- |
-- Utility function
--
-- Used both by the traversal `rewriteInlineStateMutation` and the one to rewrite function declarations
--
-- What it does is, for each expression in the sequence, prepend the corresponding let bindings `let (temp_i, scopes) = ...` in case it modifies the state,
-- then, handle the returning state accordingly.
--
-- Note that the sequence itself is NOT replaced by a `let (temp_i, scopes) = {...}`
-- This is handled, if necessary, by the caller expression traversal
--
-- If required, pushes a new local scope at the start of the sequence.
-- This is usefult when rewriting function bodies where appending this line is more convienient to be left to the caller
--
-- Additionally, it is possible to force the Sequence of always returning the pair (return_value, tail scopes)
-- this is useful when rewriting the function body, since the logic is to consider the function to always mutate the state
--
-- Note that it does not need to know the scope since types have already been inferred
--
-- In fact, this function does not create any new binding, it only adds bindings that have already been created
rewriteInlineStateMutationInSequence :: Sequence -> AnnotatedUUID -> Map.Map Identifier Bindings -> Bool -> Bool -> (Sequence, Map.Map Identifier Bindings, Bool)
rewriteInlineStateMutationInSequence (Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) scopeUUID bindingsToAdd doPushScope forceMutatesState =
  let --
      -- For each sequence item, prepend its temporary let bindings
      -- Here it forces the optional `forceMutatesState`, so that the subsequent logic will be the same unregarding that parameter
      (sequenceItems', bindingsToAdd', mutatesState) = foldr handleSeqItem ([], bindingsToAdd, forceMutatesState) sequenceItems
        where
          -- Returns True if an expression could modify the state in any way, useful in the case the expression does not have any temporary let binding associated.
          -- This happens for PUT and POST operations on the state, but not for move_to, move_from and function calls
          hasIntermediateStateMutations :: Expr -> Bool
          hasIntermediateStateMutations expr' =
            any
              ( \expr'' -> case expr'' of
                  IntermediateExprExpr (IntermediatePutLocalState {}) -> True
                  IntermediateExprExpr (IntermediatePostLocalState _ _) -> True
                  IntermediateExprExpr (IntermediatePutDereferenceLocalState {}) -> True
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
        Just sequenceEndExpr'' ->
          case getTemporaryBindingsOfExpr sequenceEndExpr'' bindingsToAdd' of
            (sortedBinds, bindingsToAdd''') ->
              if mutatesState || not (null sortedBinds)
                -- If either the sequence items or the ending expression mutate state,
                -- return the pair `(end_expr, tail scopes)` and append any temporary binding for the end expression to the sequence items
                then case sequenceEndExpr'' of
                  -- There is the case where the expression is a return. In this case, remove the `return` keyword
                  Return sequenceEndExpr''' ->
                    ( Just $ CommaExpr [fromMaybe (CommaExpr []) sequenceEndExpr''', IntermediateExprExpr $ IntermediatePopScope cpsIdentifier],
                      sequenceItems' ++ sortedBinds,
                      bindingsToAdd''',
                      True
                    )
                  -- Otherwise, just return the `(end_expr, tail scopes)`
                  _ ->
                    ( Just $ CommaExpr [sequenceEndExpr'', IntermediateExprExpr $ IntermediatePopScope cpsIdentifier],
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
              ( Just $ CommaExpr [CommaExpr [], IntermediateExprExpr $ IntermediatePopScope cpsIdentifier],
                sequenceItems',
                bindingsToAdd',
                True
              )
            -- Otherwise, do not return anything
            else (Nothing, sequenceItems', bindingsToAdd', False)

      -- Append a new local state if requested by the caller
      sequenceItems''' =
        if doPushScope
          then
            ( SequenceItemBindExpr $
                Bindings
                  { bindings = BindedSingle $ BindIdentifier cpsIdentifier (Just scopeUUID),
                    bindingsBindType = Just IntermediateTypeScopes,
                    bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePushScope cpsIdentifier
                  }
            )
              : sequenceItems''
          else sequenceItems''

      newSequence =
        Sequence
          { sequenceUses = sequenceUses,
            sequenceItems = sequenceItems''',
            sequenceEndExpr = sequenceEndExpr'
          }
   in (newSequence, bindingsToAdd'', mutatesState')

-- |
-- Utility function
--
-- Rewrites all function declarations (parameters and return type) so to add the outer state,
-- since they might mutate the global storage and it is not possible to know in advance (at least without a proper static analysis)
--
-- Also, if a parameter has to be inserted into the state, it prepends the corresponding let binding at the start of the body
rewriteFunctionDeclaration :: Root -> Set.Set AnnotatedUUID -> Map.Map Identifier Bindings -> AnnotatedUUID -> Root
rewriteFunctionDeclaration root markedVars bindingsToAdd scopeUUID = case root of
  RModule rModule@Module {moduleTopLevels} -> RModule $ rModule {moduleTopLevels = map rewriteTopLevel moduleTopLevels}
  RScript rScript@Script {scriptTopLevels} -> RScript $ rScript {scriptTopLevels = map rewriteTopLevel scriptTopLevels}
  where
    rewriteTopLevel :: TopLevel -> TopLevel
    rewriteTopLevel (TopLevelFunction tlf@Function {functionParameters, functionBody, functionReturnType}) =
      let --
          -- Extract all parameters that need to be inserted into the state
          markedParameters =
            filter
              ( \param@Parameter {parameterUUID} ->
                  case parameterUUID of
                    Just parameterUUID' -> Set.member parameterUUID' markedVars
                    Nothing -> error $ "Found function parameter without UUID: " ++ show param
              )
              functionParameters

          fst3 :: (a, b, c) -> a
          fst3 (a, _, _) = a

          -- First, rewrite the body Sequence
          -- Note that here the `bindingsToAdd` is not updated, mainly for simplicy in the code
          -- in any case it is not needed, since the usefulness of updating (removing entries) that variable comes only when rewriting nested sequences
          --
          -- Additionally, note how it forces to consider the Sequence as if it mutates the state
          -- this is to leave the called function to reqrite the ending expression so to return the state
          functionBody' =
            ( \functionBody''' ->
                fst3 $ rewriteInlineStateMutationInSequence functionBody''' scopeUUID bindingsToAdd False True
            )
              <$> functionBody

          -- Then, prepend some let bindings for pushing the new scope and the marked parameters
          functionBody'' =
            ( \functionBody'''@Sequence {sequenceItems} ->
                functionBody'''
                  { sequenceItems =
                      -- First, push the new scope in the body
                      ( SequenceItemBindExpr $
                          Bindings
                            { bindings = BindedSingle $ BindIdentifier cpsIdentifier (Just scopeUUID),
                              bindingsBindType = Just IntermediateTypeScopes,
                              bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePushScope cpsIdentifier
                            }
                      )
                        :
                        -- Then, push each marked parameter on the scope
                        map
                          ( \Parameter {parameterIdentifier} ->
                              SequenceItemBindExpr $
                                Bindings
                                  { bindings = BindedSingle $ BindIdentifier cpsIdentifier $ Just scopeUUID,
                                    bindingsBindType = Just IntermediateTypeScopes,
                                    bindingsBindExpr = Just $ IntermediateExprExpr $ IntermediatePostLocalState parameterIdentifier (Just $ NameAccessChainExpr $ LocalNameAccessChain parameterIdentifier)
                                  }
                          )
                          markedParameters
                        ++ sequenceItems
                  }
            )
              <$> functionBody'

          -- Always add the state in the return type
          functionReturnType' = case functionReturnType of
            Nothing -> Just $ TypeTuple [unitType, IntermediateTypeScopes]
            Just functionReturnType'' -> Just $ TypeTuple [functionReturnType'', IntermediateTypeScopes]

          -- Append the scope as last parameter
          functionParameters' =
            functionParameters
              ++ [ Parameter
                     { parameterIdentifier = cpsIdentifier,
                       parameterType = IntermediateTypeScopes,
                       parameterUUID = Just scopeUUID
                     }
                 ]
       in TopLevelFunction
            tlf
              { functionParameters = functionParameters',
                functionBody = functionBody'',
                functionReturnType = functionReturnType'
              }
    rewriteTopLevel tl = tl

-- |
-- Given an AST and the set of variables that need to be inserted into the local scope,
-- rewrites the usages of those variables with (intermediate) AST nodes that act on the explicit local scope
--
-- See the comments on the inner function calls for additional logic
--
-- NOTE: this step requires the type witnesses to be already translated.
-- In fact, it needs the to correctly rewrite the global storage operators
--
-- ## The following is a summary of the implemented functionalities:
--
--    - Function's signature is updated to accept and return the outer scopes
--    - A local scope is pushed at the beginning of each Sequence
--    - Function parameters are inserted (if needed) in the local scope at the start of the function
--    - Assignments are rewritten as PUT operations on the state
--    - `let` bindings are rewritten as POST operations on the state
--    - References and dereferences are rewitten so that always apply to variables that have been inserted in the scope
--    - A function call or a sequence that modifies the state are extracted when inlined, associating a temporary variable to them
--    - Type inference is used when GETting from the state and supports the most common cases
--
-- ## TODO: The following is instead a list of functionalities currently not implemented or that can be extended
--
--    - A variable that is referenced outside will need to be lifted
--    - if-then-else are currently not aligned in the return type of the branches
--    - Function calls and structs namings only apply to local names
--    - Tuple bindings, destructuring of structs and tuple assignments in the cases where at least one variable has to be inserted in the scope are not supported
--    - Dot accesses expect the left part to always be a struct, inline values are currently not supported
addLocalScopeInRoot :: [Root] -> Set.Set AnnotatedUUID -> State AnnotatedUUID [Root]
addLocalScopeInRoot roots markedVars = do
  let scopeUUID = -1
  -- It is safe to perform all intermediate passes on each Root, since only local variables are affected
  -- This is more performant than performing each intermediate pass on all roots, then the second on all roots and so on
  -- TODO: Also missing liftings
  iterateWithOthers (addLocalScopeIterator scopeUUID) roots
  where
    addLocalScopeIterator :: AnnotatedUUID -> Root -> [Root] -> State AnnotatedUUID Root
    addLocalScopeIterator scopeUUID root otherRoots = do
      let firstPass :: Root = rewriteAssignments root otherRoots scopeUUID
      let secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass () otherRoots
      let thirdPass :: Root = fst $ traverseRootPostOrder (rewriteVars markedVars) traversalIdentity secondPass () otherRoots
      let fourthPass :: Root = fst $ traverseRootPostOrder traversalIdentity (rewriteLetBinds markedVars scopeUUID) thirdPass () otherRoots
      currUUID <- get
      let (fifthPass, (updatedUUID, bindingsToAdd)) = traverseRootPostOrder (rewriteInlineStateMutation scopeUUID) traversalIdentity fourthPass (currUUID, Map.empty) otherRoots
      put updatedUUID
      let sixthPass = rewriteFunctionDeclaration fifthPass markedVars bindingsToAdd scopeUUID
      return sixthPass