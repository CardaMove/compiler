module Move.Translations.Utils where

import Data.Map qualified as Map
import Move.AST

-- | Dummy type representing any type that is not possible to infer
unknownType :: Type
unknownType = TypeConstructor (LocalNameAccessChain $ Identifier "UNKNOWN_TYPE") []

-- | bool type
booleanType :: Type
booleanType = TypeConstructor (LocalNameAccessChain $ Identifier "bool") []

-- | Numeric type.
-- For simplicity, it represents the "bigger" of the numeric types in Move, `u256`
numericType :: Type
numericType = TypeConstructor (LocalNameAccessChain $ Identifier "u256") []

-- | Represents a local scope
type Scope = Map.Map Identifier (Maybe Type)

-- |
-- Checks if the identifier is present in any of the input scopes
isIdentifierInScope :: Identifier -> [Scope] -> Bool
isIdentifierInScope _ [] = False
isIdentifierInScope ident (x : xs) = Map.member ident x || isIdentifierInScope ident xs

-- |
-- Returns the inferred type of an identifier present in the scope.
--
-- Throws an error if the identifier is not present in the scope
getIdentifierTypeFromScope :: Identifier -> [Scope] -> Maybe Type
getIdentifierTypeFromScope ident [] = error ("Cannot get identifier type. Not in scope: " ++ show ident)
getIdentifierTypeFromScope ident (x : xs) = case Map.lookup ident x of
  -- The identifier is not in this scope, proceed recursively
  Nothing -> getIdentifierTypeFromScope ident xs
  -- The identifier is in this scope, return its type (if available)
  Just maybeType -> maybeType

-- |
-- Utility function. Given a Maybe value, return that value of not Nothing, or a default one otherwise
getValueOrDefault :: Maybe val -> val -> val
getValueOrDefault Nothing def = def
getValueOrDefault (Just val) _ = val

-- |
-- Given a Use, returns all the alias identifiers.
--
-- Also handles the use members and the Self member
getUseIdentifiers :: Use -> [Identifier]
getUseIdentifiers use = case use of
  Use {useIdentifier, useAlias, useMembers = []} -> [getIdentifier useAlias useIdentifier]
  Use {useIdentifier, useMembers = [UseMember {useMemberIdentifier = Identifier "Self", useMemberUseAlias}]} -> [getIdentifier useMemberUseAlias useIdentifier]
  Use {useMembers = [UseMember {useMemberIdentifier, useMemberUseAlias}]} -> [getIdentifier useMemberUseAlias useMemberIdentifier]
  use'@Use {useMembers = x : xs} -> getUseIdentifiers use' {useMembers = [x]} ++ getUseIdentifiers use' {useMembers = xs}
  where
    getIdentifier :: Maybe Identifier -> Identifier -> Identifier
    getIdentifier Nothing def = def
    getIdentifier (Just ident) _ = ident

-- |
-- Given a single bind, returns all its binded identifiers as they are named in the AST
getBindIdentifiers :: Bind -> [Identifier]
getBindIdentifiers bind =
  let getBindIdentifiersHelper :: [BindedField] -> [Identifier]
      getBindIdentifiersHelper = concatMap f
        where
          f (BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing}) = [bindFieldIdentifier]
          f (BindedField {bindFieldInnerBind = Just innerBind}) = getBindIdentifiers innerBind
   in case bind of
        (BindIdentifier ident _) -> [ident]
        (BindNamedStruct (BindedNamedStruct {bnsFields = BindedFields {bindedFields}})) -> getBindIdentifiersHelper bindedFields
        (BindPositionalStruct (BindedPositionalStruct {bpsFields = BindedFields {bindedFields}})) -> getBindIdentifiersHelper bindedFields

-- |
-- Given a binding, returns a list with every binded identifier along with its type, if it could be inferred
inferBindingsTypes :: Bindings -> [(Identifier, Maybe Type)]
--  For a single bind, infer its type
inferBindingsTypes (Bindings {bindings = BindedSingle bind, bindingsBindType, bindingsBindExpr}) = inferBindType bind bindingsBindType bindingsBindExpr
--  For a tuple bind, if neither types nor expression is specified, types can not be inferred
inferBindingsTypes (Bindings {bindings = BindedTuple binds, bindingsBindType = Nothing, bindingsBindExpr = Nothing}) =
  let bindsIdentifiers = concatMap getBindIdentifiers binds
   in map (,Nothing) bindsIdentifiers
--  For a tuple bind, if types are specified, they must be a tuple of types
inferBindingsTypes (Bindings {bindings = BindedTuple binds, bindingsBindType = Just (TypeTuple types)}) =
  if length binds /= length types
    then error ("When inferring binding types: number of bindings is different from number of types. " ++ show binds ++ ", " ++ show types)
    else concatMap (\(bind, t) -> inferBindType bind (Just t) Nothing) (zip binds types)
--  If instead types are not specified, they must be inferred from the expression, which should be a tuple
inferBindingsTypes (Bindings {bindings = BindedTuple binds, bindingsBindType = Nothing, bindingsBindExpr = Just (CommaExpr exprs)}) =
  if length binds /= length exprs
    then error ("When inferring binding types: number of bindings is different from number of expression. " ++ show binds ++ ", " ++ show exprs)
    else concatMap (\(bind, expr) -> inferBindType bind Nothing (Just expr)) (zip binds exprs)
--  A tuple bind can destructure a function result. This is not currently inferred
inferBindingsTypes (Bindings {bindings = BindedTuple binds, bindingsBindType = Nothing, bindingsBindExpr = Just (PositionalStructExprOrFunctionCallExpr _)}) =
  let bindsIdentifiers = concatMap getBindIdentifiers binds
   in map (,Nothing) bindsIdentifiers
--  It is not possible to specify a non-tuple types for a tuple binding
inferBindingsTypes (Bindings {bindings = BindedTuple _, bindingsBindType = Just _}) = error "Found a tuple binding typed with a non-tuple type"
-- A tuple bind with any other expression is invalid
inferBindingsTypes (Bindings {bindings = BindedTuple _, bindingsBindType = Nothing, bindingsBindExpr = Just _}) = error "Found a tuple binding assigned with a non-tuple expression"

-- |
-- Give a single bind and its corresponding type or expression, tries to infer the type of all the binded identifiers
inferBindType :: Bind -> Maybe Type -> Maybe Expr -> [(Identifier, Maybe Type)]
--  It is not possible to infer the type of an identifier alone (or in another way, it can be any type)
inferBindType (BindIdentifier ident _) Nothing Nothing = [(ident, Nothing)]
--  Identifier with type annotation
inferBindType (BindIdentifier ident _) identType@(Just _) _ = [(ident, identType)]
--  Identifier with corresponding binary expression
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Or _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (And _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Eq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Neq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Lt _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Gt _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Leq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Geq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (BitwiseOr _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (BitwiseXor _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (BitwiseAnd _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (ShiftLeft _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (ShiftRight _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Add _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Sub _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Mult _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Div _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (BinaryOpExprExpr (Mod _ _))) = [(ident, Just numericType)]
--  An assignment has unit type
inferBindType (BindIdentifier ident _) Nothing (Just (AssignmentExpr _)) = [(ident, Just $ TypeTuple [])]
--  Identifier with unary expression
inferBindType (BindIdentifier ident _) Nothing (Just (UnaryOpExpr (Negation _))) = [(ident, Just numericType)]
--  Identifier with a typed expression or a casting
inferBindType (BindIdentifier ident _) Nothing (Just (TypedExprTerm (TypedExpr {typedExprType}))) = [(ident, Just typedExprType)]
inferBindType (BindIdentifier ident _) Nothing (Just (CastingTerm (Casting {castingType}))) = [(ident, Just castingType)]
-- while loops have unit type
inferBindType (BindIdentifier ident _) Nothing (Just (WhileTerm _)) = [(ident, Just $ TypeTuple [])]
-- Basic inference with literal values
inferBindType (BindIdentifier ident _) Nothing (Just (ValueLiteral (Numerical _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident _) Nothing (Just (ValueLiteral (Address _))) = [(ident, Just $ TypeConstructor (LocalNameAccessChain $ Identifier "address") [])]
inferBindType (BindIdentifier ident _) Nothing (Just (ValueLiteral (Boolean _))) = [(ident, Just booleanType)]
-- For all other expressions, either is it needed to know the scope or have any type
inferBindType bind _ _ = map (,Nothing) (getBindIdentifiers bind)