module Move.Translations.Mutation (markMutabilityInRoot) where

import Data.Generics.Uniplate.Data (children)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set qualified as Set
import Move.AST (AnnotatedUUID, Assignment (Assignment, assignmentLeft, assignmentRight), DotOrIndexChain (DotAccess, dotAccessLeft), Expr (AssignmentExpr, DotOrIndexChainExpr, NameAccessChainExpr, UnaryOpExpr), Identifier, NameAccessChain (LocalNameAccessChain), Root, UnaryExpr (MutableReference))
import Move.Translations.TraversalUtils (TraverseExprMapper, traverseRootPostOrder)
import Move.Translations.Utils (VariableAnnotations (VariableAnnotations), getIdentifierFromScopes)

-- |
-- Results of the function `markMutabilityInRoot`
--
-- Contains the (UUIDs of the) variables that are (or might be) mutated,
-- which variables are assigned to each mutated variable
data MutabilityResult
  = MutabilityResult
  { mutatedVars :: [AnnotatedUUID],
    assignedToKey :: Map.Map AnnotatedUUID [AnnotatedUUID]--,
    -- referencedExprs :: Set.Set Expr
  }

-- |
-- Given a module or script, inspects all the variables and marks the one that are (or could be) mutated in any way,
-- thus needing to be handled with a separate state variable
markMutabilityInRoot :: Root -> MutabilityResult
markMutabilityInRoot root =
  let (_, mutRes) = traverseRootPostOrder f root emptyRes
   in mutRes
  where
    emptyRes = MutabilityResult {mutatedVars = [], assignedToKey = Map.empty}
    f :: TraverseExprMapper MutabilityResult
    -- If `&mut a` is found, mark `a` as (possibly) mutated
    f expr@(UnaryOpExpr (MutableReference (NameAccessChainExpr (LocalNameAccessChain ident)))) scopes mutRes@MutabilityResult {mutatedVars} =
      case getIdentifierFromScopes ident scopes of
        VariableAnnotations (Just identUUID) _ -> (expr, mutRes {mutatedVars = identUUID : mutatedVars})
        -- FIXME: Only do this if `a` has a UUID (probably all symbols should have an UUID). Also applies to the other cases of `f`
        _ -> (expr, mutRes)
    -- If `a.[b.c] = ...` is found, mark `a` as mutated, and every identifier on the right side as assigned to `a`
    f expr@(AssignmentExpr (Assignment {assignmentLeft, assignmentRight})) scopes mutRes@MutabilityResult {mutatedVars, assignedToKey} =
      let leftmostIdent :: Identifier = pickLeft assignmentLeft
            where
              pickLeft :: Expr -> Identifier
              pickLeft (NameAccessChainExpr (LocalNameAccessChain ident)) = ident
              pickLeft (DotOrIndexChainExpr (DotAccess {dotAccessLeft})) = pickLeft dotAccessLeft
              pickLeft _ = error "Expected a dot chain expression (or a local name) as left value of an assignment"

          -- Here we have the UUID of the leftmost identifier (meaning the identifier of `a`)
          leftmostUUID :: Maybe AnnotatedUUID = case getIdentifierFromScopes leftmostIdent scopes of
            VariableAnnotations maybeUUID _ -> maybeUUID

          mutRes' = case leftmostUUID of
            -- If a UUID is found, updated the results
            Just leftmostUUID' ->
              -- Find all the UUIDs assigned to `a` and add them to the results
              -- also mark the UUID of `a` as mutated
              let existingAssignedTo :: [AnnotatedUUID] = fromMaybe [] $ Map.lookup leftmostUUID' assignedToKey
                  assignedTo :: [AnnotatedUUID] = findUUIDsInRightValue assignmentRight
                    where
                      findUUIDsInRightValue :: Expr -> [AnnotatedUUID]
                      findUUIDsInRightValue (NameAccessChainExpr (LocalNameAccessChain ident)) =
                        case getIdentifierFromScopes ident scopes of
                          VariableAnnotations (Just identUUID) _ -> [identUUID]
                          _ -> []
                      findUUIDsInRightValue expr' = concatMap findUUIDsInRightValue (children expr')
               in mutRes {mutatedVars = leftmostUUID' : mutatedVars, assignedToKey = Map.insert leftmostUUID' (assignedTo ++ existingAssignedTo) assignedToKey}
            Nothing -> mutRes
       in (expr, mutRes')
    f _ _ _ = error "TODO:"