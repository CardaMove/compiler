module Move.Translations.Utils where

import Control.Monad.State qualified as State
import Data.Generics.Uniplate.Data (descendM)
import Data.List (mapAccumL)
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
--      When a sequence is found, call the specific traversal for it and then invoke f on the resulting Sequence
--      Note that outside of a SequenceExpr, the scope is not modified
traverseExprPostOrder f (SequenceExpr sequence') scopes state =
  let (sequence'', state') = traverseSequencePostOrder f sequence' (Map.empty : scopes) state
   in f (SequenceExpr sequence'') scopes state'
--      When any other expression is found, recursively descend with `descendM` to keep the state
traverseExprPostOrder f expr scopes state =
  let (expr''', state''') = State.runState (descendM fDesc expr) state
        where
          fDesc expr' = do
            state' <- State.get
            let (expr'', state'') = traverseExprPostOrder f expr' scopes state'
            State.put state''
            return expr''
   in f expr''' scopes state'''

-- |
-- Similar to `traverseExprPostOrder` but restricted to a Sequence
-- The topmost scope is considered the local one, so it must be provided
traverseSequencePostOrder :: (Expr -> [Scope] -> state -> (Expr, state)) -> Sequence -> [Scope] -> state -> (Sequence, state)
traverseSequencePostOrder _ _ [] _ = error "Cannot traverse Sequence with no scopes"
traverseSequencePostOrder f (Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) (localScope : outerScopes) state =
  -- First, add the uses to the local scope
  let usesIdentifiers = concatMap getUseIdentifiers sequenceUses
      usesScope = Map.fromList (map (,Nothing) usesIdentifiers)
      scopesBeforeSeqItems = Map.union usesScope localScope : outerScopes
      -- Then, recursively traverse the sequence items, and collect the new scope and state
      ((scopesAfterSeqItems, stateAfterSeqItems), sequenceItems') = mapAccumL fAcc (scopesBeforeSeqItems, state) sequenceItems
        where
          fAcc ([], _) _ = error "Cannot traverse SequenceItem with no scopes"
          -- When an expression is encountered, traverse it
          fAcc (scopesBeforeExpr, stateBeforeExpr) (SequenceItemExpr expr) =
            let (expr', stateAfterTraversingExpr) = traverseExprPostOrder f expr scopesBeforeExpr stateBeforeExpr
             in -- It can be noticed that traversing an expression has not changed the scope
                ((scopesBeforeExpr, stateAfterTraversingExpr), SequenceItemExpr expr')
          -- When a binding is encountered in the sequence:
          fAcc (scopesBeforeBind@(localScopeBeforeBind : outerScopesBeforeBind), stateBeforeBind) (SequenceItemBindExpr bindings@(Bindings {bindingsBindExpr})) =
            -- first, traverse the binded expression and retrieve the new expression and state
            let (traversedBindExpr, stateAfterTraversingBindExpr) = case bindingsBindExpr of
                  Nothing -> (Nothing, stateBeforeBind)
                  Just bindExpr ->
                    let (mappedBindExpr, state') = traverseExprPostOrder f bindExpr scopesBeforeBind stateBeforeBind
                     in (Just mappedBindExpr, state')
                -- Note: here should be called any function that would map the binding
                -- Then, add the binded identifiers to the local scope
                bindScopes = Map.fromList $ inferBindingsTypes bindings
                localScopeAfterBind = Map.union bindScopes localScopeBeforeBind
             in ( (localScopeAfterBind : outerScopesBeforeBind, stateAfterTraversingBindExpr),
                  SequenceItemBindExpr $
                    bindings {bindingsBindExpr = traversedBindExpr}
                )

      -- Then, traverse the ending expression
      (sequenceEndExpr', stateAfterEndExpr) = case sequenceEndExpr of
        Nothing -> (Nothing, stateAfterSeqItems)
        Just endExpr ->
          let (mappedEndExpr, state') = traverseExprPostOrder f endExpr scopesAfterSeqItems stateAfterSeqItems
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
-- Performs a post-order traversal of an entire module.
--
-- Invokes a function for each encountered expression (see `traverseExprPostOrder`), and returns the resulting Module and state
traverseModulePostOrder :: (Expr -> [Scope] -> state -> (Expr, state)) -> Module -> state -> (Module, state)
traverseModulePostOrder = error "TODO:"

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

-- | Helper function for `getBindIdentifiers`
getBindIdentifiersHelper :: [BindedField] -> [Identifier]
getBindIdentifiersHelper = concatMap f
  where
    f (BindedField {bindFieldIdentifier, bindFieldInnerBind = Nothing}) = [bindFieldIdentifier]
    f (BindedField {bindFieldInnerBind = Just innerBind}) = getBindIdentifiers innerBind

-- |
-- Given a single bind, returns all its binded identifiers as they are named in the AST
getBindIdentifiers :: Bind -> [Identifier]
getBindIdentifiers (BindIdentifier ident) = [ident]
getBindIdentifiers (BindNamedStruct (BindedNamedStruct {bnsFields = BindedFields {bindedFields}})) = getBindIdentifiersHelper bindedFields
getBindIdentifiers (BindPositionalStruct (BindedPositionalStruct {bpsFields = BindedFields {bindedFields}})) = getBindIdentifiersHelper bindedFields

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
inferBindType (BindIdentifier ident) Nothing Nothing = [(ident, Nothing)]
--  Identifier with type annotation
inferBindType (BindIdentifier ident) identType@(Just _) _ = [(ident, identType)]
--  Identifier with corresponding binary expression
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Or _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (And _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Eq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Neq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Lt _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Gt _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Leq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Geq _ _))) = [(ident, Just booleanType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (BitwiseOr _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (BitwiseXor _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (BitwiseAnd _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (ShiftLeft _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (ShiftRight _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Add _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Sub _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Mult _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Div _ _))) = [(ident, Just numericType)]
inferBindType (BindIdentifier ident) Nothing (Just (BinaryOpExprExpr (Mod _ _))) = [(ident, Just numericType)]
--  An assignment has unit type
inferBindType (BindIdentifier ident) Nothing (Just (AssignmentExpr _)) = [(ident, Just $ TypeTuple [])]
--  Identifier with unary expression
inferBindType (BindIdentifier ident) Nothing (Just (UnaryOpExpr (Negation _))) = [(ident, Just numericType)]
--  Identifier with a typed expression or a casting
inferBindType (BindIdentifier ident) Nothing (Just (TypedExprTerm (TypedExpr {typedExprType}))) = [(ident, Just typedExprType)]
inferBindType (BindIdentifier ident) Nothing (Just (CastingTerm (Casting {castingType}))) = [(ident, Just castingType)]
-- while loops have unit type
inferBindType (BindIdentifier ident) Nothing (Just (WhileTerm _)) = [(ident, Just $ TypeTuple [])]
-- For all other expressions, either is it needed to know the scope or have any type
inferBindType bind _ _ = map (,Nothing) (getBindIdentifiers bind)