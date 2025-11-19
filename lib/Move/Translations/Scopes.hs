module Move.Translations.Scopes where

import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traverseRootPostOrder, traversalBindingsIdentity)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), getIdentifierFromScopes, inferExprType)

newtype LocalScopeAnalysis = LocalScopeAnalysis
  { markedVars :: Set.Set (AnnotatedUUID, Type)
  }

-- TODO: Rewrite to return LocalScopeAnalysis
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


addLocalScopeInRoot :: Root -> LocalScopeAnalysis -> Root
addLocalScopeInRoot root LocalScopeAnalysis {markedVars} =
  let -- Given either a local identifier or a dot access chain `&[mut] a[.b.c]`,
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

      -- Given any dereference, rewrites the node as a getter from the state
      -- This passage is mainly used to preserve the resulting type, that would not be inferrable anymore after the AST rewrite
      mapRightValueDeref :: Expr -> Type -> Expr
      mapRightValueDeref expr exprType = IntermediateExprExpr $ IntermediateDereferenceLocalState expr exprType


      -- First pass: rewrite assignments (only left value) as pseudo let binding
      -- "pseudo" is due to the fact that are still expressions rather than `let` bindings in the AST,
      -- So this intermediate AST will be malformed
      -- TODO: Solve by using a custom traversal that works on Sequence items and produces actual let bindings
      rewriteAssignments :: TraversalMapper Expr ()
      rewriteAssignments expr scopes state = error "TODO:"

      -- TODO: Must be called after rewriting assignment to prevent updating derefs on left value
      -- Second pass: rewrite references on right value as getters
      -- Also rewrite dereferences on right value
      rewriteRefs :: TraversalMapper Expr ()
      rewriteRefs expr@(UnaryOpExpr (ImmutableReference referencedExpr)) scopes state = (mapRightValueRef referencedExpr $ inferExprType expr scopes, state)
      rewriteRefs expr@(UnaryOpExpr (MutableReference referencedExpr)) scopes state = (mapRightValueRef referencedExpr $ inferExprType expr scopes, state)
      rewriteRefs expr@(UnaryOpExpr (Dereference dereferencedExpr)) scopes state = (mapRightValueDeref dereferencedExpr $ inferExprType expr scopes, state)
      rewriteRefs expr _scopes state = (expr, state)

      -- TODO: Third pass: rewrite variables on right value (must be done after references to avoid conflicts)
      -- TODO: Fourth pass: rewrite `let` bindings (must be done as last so to preserve type inference)

      firstPass :: Root = fst $ traverseRootPostOrder rewriteAssignments traversalBindingsIdentity root ()
      secondPass :: Root = fst $ traverseRootPostOrder rewriteRefs traversalBindingsIdentity firstPass ()
   in secondPass