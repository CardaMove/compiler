module Move.Translations.Utils where

import Data.Map qualified as Map
import Move.AST

unknownType :: Type
unknownType = TypeConstructor (LocalNameAccessChain $ Identifier "UNKNOWN_TYPE") []

booleanType :: Type
booleanType = TypeConstructor (LocalNameAccessChain $ Identifier "bool") []

numericType :: Type
numericType = TypeConstructor (LocalNameAccessChain $ Identifier "u256") []

type Scope = Map.Map Identifier (Maybe Type)

-- |
-- Performs a post-order traversal of a tree of expressions.
--
-- Accepts as input a function that takes the current expression, all the accumulated scopes at that node,
-- and a custom state. The function should return an expression to substitute and a new state
traverseExprPostOrder :: (Expr -> [Scope] -> state -> (Expr, state)) -> Expr -> [Scope] -> state -> (Expr, state)
traverseExprPostOrder f expr scopes state = error "TODO:"

-- |
-- Checks if the identifier is present in any of the input scopes
isIdentifierInScope :: Identifier -> [Scope] -> Bool
isIdentifierInScope = error "TODO:"

-- |
-- Returns the inferred type of an identifier present in the scope.
--
-- Throws an error if the identifier is not present in the scope
getIdentifierTypeFromScope :: Identifier -> [Scope] -> Maybe Type
getIdentifierTypeFromScope = error "TODO:"

-- |
-- Utility function. Given a Maybe value, return that value of not Nothing, or a default one otherwise
getValueOrDefault :: Maybe val -> val -> val
getValueOrDefault Nothing def = def
getValueOrDefault (Just val) _ = val
