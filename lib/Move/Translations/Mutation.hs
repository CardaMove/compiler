module Move.Translations.Mutation (markMutabilityInRoot, MutabilityResult (..)) where

import Data.Generics.Uniplate.Data (children)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Maybe (fromMaybe)
import Move.AST (AnnotatedUUID, Assignment (Assignment, assignmentLeft, assignmentRight), DotOrIndexChain (DotAccess, dotAccessLeft), Expr (AssignmentExpr, DotOrIndexChainExpr, NameAccessChainExpr, UnaryOpExpr), Identifier, NameAccessChain (LocalNameAccessChain), Root, UnaryExpr (MutableReference), Type (TypeImmutableRef, TypeMutableRef))
import Move.Translations.TraversalUtils (TraverseExprMapper, traverseRootPostOrder)
import Move.Translations.Utils (VariableAnnotations (VariableAnnotations), getIdentifierFromScopes, inferExprType)

-- |
-- Results of the function `markMutabilityInRoot`
--
-- Contains the (UUIDs of the) variables that are (or might be) mutated,
-- which variables are assigned to each mutated variable
data MutabilityResult
  = MutabilityResult
  { mutatedVars :: Set.Set AnnotatedUUID,
    assignedToKey :: Map.Map AnnotatedUUID (Set.Set AnnotatedUUID),
    mutatedRefs :: Set.Set AnnotatedUUID
  }

-- |
-- Given a module or script, inspects all the variables and marks the one that are (or could be) mutated in any way,
-- thus needing to be handled with a separate state variable
markMutabilityInRoot :: Root -> MutabilityResult
markMutabilityInRoot root = snd $ traverseRootPostOrder f root emptyRes
  where
    emptyRes = MutabilityResult {mutatedVars = Set.empty, assignedToKey = Map.empty, mutatedRefs = Set.empty}
    f :: TraverseExprMapper MutabilityResult
    -- If `&mut a` is found, mark `a` as (possibly) mutated
    f expr@(UnaryOpExpr (MutableReference (NameAccessChainExpr (LocalNameAccessChain ident)))) scopes mutRes@MutabilityResult {mutatedVars} =
      case getIdentifierFromScopes ident scopes of
        VariableAnnotations (Just identUUID) _ -> (expr, mutRes {mutatedVars = Set.insert identUUID mutatedVars})
        -- Throw an error if the identifier is 
        _ -> error $ "Found identifier in right value without annotations when performing mutability analysis: " ++ show ident
    -- If `a.[b.c] = ...` is found, mark `a` as mutated, and every identifier on the right side as assigned to `a`
    f expr@(AssignmentExpr (Assignment {assignmentLeft, assignmentRight})) scopes MutabilityResult {mutatedVars, assignedToKey, mutatedRefs} =
      let leftmostIdent :: Identifier = pickLeft assignmentLeft
            where
              pickLeft :: Expr -> Identifier
              pickLeft (NameAccessChainExpr (LocalNameAccessChain ident)) = ident
              pickLeft (DotOrIndexChainExpr (DotAccess {dotAccessLeft})) = pickLeft dotAccessLeft
              pickLeft _ = error $ "Expected a dot chain expression (or a local name) as left value of an assignment: " ++ show assignmentLeft

          -- Here we have the UUID of the leftmost identifier (meaning the identifier of `a`)
          leftmostUUID :: Maybe AnnotatedUUID = case getIdentifierFromScopes leftmostIdent scopes of
            VariableAnnotations maybeUUID _ -> maybeUUID

          mutRes' = case leftmostUUID of
            -- If a UUID is found, updated the results
            Just leftmostUUID' ->
              -- Find all the UUIDs assigned to `a` and add them to the results
              let existingAssignedTo :: Set.Set AnnotatedUUID = fromMaybe Set.empty $ Map.lookup leftmostUUID' assignedToKey
                  assignedTo :: Set.Set AnnotatedUUID = findUUIDsInRightValue assignmentRight
                    where
                      concatSet :: Ord b => (a -> Set.Set b) -> [a] -> Set.Set b
                      concatSet _ [] = Set.empty
                      concatSet mapper (x:xs) = Set.union (mapper x) $ concatSet mapper xs

                      findUUIDsInRightValue :: Expr -> Set.Set AnnotatedUUID
                      findUUIDsInRightValue (NameAccessChainExpr (LocalNameAccessChain ident)) =
                        case getIdentifierFromScopes ident scopes of
                          VariableAnnotations (Just identUUID) _ -> Set.singleton identUUID
                          _ -> Set.empty
                      findUUIDsInRightValue expr' = concatSet findUUIDsInRightValue (children expr')

                    -- If the whole right value is not a reference, consider `a` as a mutated variable,
                    -- otherwise, consider it as a mutated reference
                    -- The distinction is useful so to modify the AST without the need of performing a custom type inference on let bindings
                    -- since the type of the left value is already known (or better, it is known if it is a reference or not)
                  (mutatedVars', mutatedRefs') = case inferExprType assignmentRight scopes of
                      TypeImmutableRef _ -> (mutatedVars, Set.insert leftmostUUID' mutatedRefs)
                      TypeMutableRef _ -> (mutatedVars, Set.insert leftmostUUID' mutatedRefs)
                      _ -> (Set.insert leftmostUUID' mutatedVars, mutatedRefs)

               in MutabilityResult {mutatedVars = mutatedVars', assignedToKey = Map.insert leftmostUUID' (Set.union assignedTo existingAssignedTo) assignedToKey, mutatedRefs = mutatedRefs'}
            Nothing -> error $ "Found leftmost identifier in left value without annotations when performing mutability analysis: " ++ show leftmostIdent
       in (expr, mutRes')
    -- TODO: Should also update "assignedTo" for the let bindings
    -- TODO: If the right value is a reference to an expression, handle it during update of AST
    f expr _ mutRes = (expr, mutRes)