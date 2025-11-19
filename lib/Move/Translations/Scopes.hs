module Move.Translations.Scopes where

import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (traverseRootPostOrder, TraversalMapper)
import Move.Translations.Utils (VariableAnnotations(VariableAnnotations), getIdentifierFromScopes, Scope)


markVariablesForLocalScope :: Root -> Set.Set AnnotatedUUID
markVariablesForLocalScope root = snd $ traverseRootPostOrder exprMapper bindsMapper root Set.empty
  where
    insertMaybe :: Ord a => Maybe a -> Set.Set a -> Set.Set a
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