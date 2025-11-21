module Move.Translations.Scopes where

import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), getIdentifierFromScopes, inferExprType)

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
-- Given an AST and the set of variables that need to be inserted into the local scope,
-- rewrites the usages of those variables with (intermediate) AST nodes that act on the explicit local scope
addLocalScopeInRoot :: Root -> Set.Set AnnotatedUUID -> Root
addLocalScopeInRoot root markedVars =
  let -- Given a reference to either a local identifier or a dot access chain `&[mut] a[.b.c]`,
      -- rewrites the node as an high level reference on the state, also preserving the resulting type
      mapRightValueRef :: Expr -> Type -> Expr
      mapRightValueRef (NameAccessChainExpr (LocalNameAccessChain ident)) identType = IntermediateExprExpr $ IntermediateReferenceLocalState [ident] identType
      mapRightValueRef expr@(DotOrIndexChainExpr _) identType = IntermediateExprExpr $ IntermediateReferenceLocalState (getDotAccessChain expr) identType
        where
          getDotAccessChain :: Expr -> [Identifier]
          getDotAccessChain (NameAccessChainExpr (LocalNameAccessChain ident)) = [ident]
          getDotAccessChain (DotOrIndexChainExpr DotAccess {dotAccessLeft, dotAccessRight}) = dotAccessRight : getDotAccessChain dotAccessLeft
          -- TODO: For now, only simple dot access chains like `a[.b.c]` are supported, but they might be inline values
          getDotAccessChain expr' = error $ "More complex dot access chain not supported: " ++ show expr'
      -- TODO: Similarly, only references to variables or dot access are supported, but can be inline values
      mapRightValueRef expr _ = error $ "More complex reference not supported: " ++ show expr

      -- First pass: rewrite assignments (only left value) as pseudo let binding
      -- "pseudo" is due to the fact that are still expressions rather than `let` bindings in the AST,
      -- So this intermediate AST will be malformed
      -- TODO: Solve by using a custom traversal that works on Sequence items and produces actual let bindings
      rewriteAssignments :: TraversalMapper Expr ()
      rewriteAssignments expr scopes state = error "TODO:"

      -- Second pass: rewrite references on right value as getters
      -- (must be called after rewriting assignment to prevent updating derefs on left value)
      rewriteRefs :: TraversalMapper Expr ()
      rewriteRefs expr@(UnaryOpExpr (ImmutableReference referencedExpr)) scopes state = (mapRightValueRef referencedExpr $ inferExprType expr scopes, state)
      rewriteRefs expr@(UnaryOpExpr (MutableReference referencedExpr)) scopes state = (mapRightValueRef referencedExpr $ inferExprType expr scopes, state)
      -- Also rewrite dereferences on right value
      -- This passage is mainly used to preserve the resulting type, that would not be inferrable anymore after the AST rewrite
      rewriteRefs expr@(UnaryOpExpr (Dereference dereferencedExpr)) scopes state = (IntermediateExprExpr $ IntermediateDereferenceLocalState dereferencedExpr $ inferExprType expr scopes, state)
      rewriteRefs expr _scopes state = (expr, state)

      -- Third pass: rewrite variables on right value, such as `a[.b.c]`
      -- (must be done after references to avoid conflicts)
      -- Note that the dot access chain is already handled with the leftmost identifier
      -- This passage is mainly used to preserve the resulting type, so to allow for an explicit cast on the translated AST
      rewriteVars :: TraversalMapper Expr ()
      rewriteVars expr@(NameAccessChainExpr (LocalNameAccessChain ident)) scopes state =
        case getIdentifierFromScopes ident scopes of
          VariableAnnotations (Just identUUID) identType ->
            -- Replace the variable only if it is marked as such
            if Set.member identUUID markedVars
              then (IntermediateExprExpr $ IntermediateGetLocalState [ident] identType, state)
              else (expr, state)
          VariableAnnotations Nothing _ -> error $ "Found identifier without UUID when adding local state: " ++ show expr
      rewriteVars expr _scopes state = (expr, state)

      -- Fourth pass: rewrite `let` bindings
      -- (must be done as last so to preserve type inference)
      -- TODO: also, only for variables that need to be inserted in the local scope
      rewriteLetBinds :: TraversalMapper Bindings ()
      rewriteLetBinds binds _scopes state = (binds, state)

      firstPass :: Root = fst $ traverseRootPostOrder rewriteAssignments traversalIdentity root ()
      secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalIdentity firstPass ()
      thirdPass :: Root = fst $ traverseRootPostOrder rewriteVars traversalIdentity secondPass ()
      fourthPass :: Root = fst $ traverseRootPostOrder traversalIdentity rewriteLetBinds thirdPass ()

      -- TODO: Also create and manage the variables for scopes: creation and return of outerscopes,
      -- passing and retrieving scopes to functions that have references
      -- or to sequences that have assignments (or references)
   in fourthPass