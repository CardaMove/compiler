module Move.Translations.TypeWitness (translateTParamsInRoot, toSnake) where

import Control.Monad.State
import Data.Char (toLower)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Unicode (isUpper)
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils

-- |
-- Given an AST, rewrites all type parameters in function declarations as an additional
-- function parameter acting as type witness and adds the corresponding arguments to function calls.
--
-- Prepends to their name a `tw_` to avoid name clashes.
--
-- NOTE: It is needed by the logic in `rewriteFunctionCalls` that the type witnesses whould be named identical to the corresponding type parameters
--
-- Additionally, rewrites all occurrences of type parameters in the body of the function.
--
-- Note that this step also handles global storage operators, since they are still represented as function calls and not `IntermediateExprExpr` nodes
translateTParamsInRoot :: [Root] -> State Int [Root]
translateTParamsInRoot roots = do
  -- Rewrite the function and struct definitions
  roots'' <- iterateWithOthers topLevelIterator roots
  -- Then, rewrite the function calls
  -- Note that the reason why two distinct iterations are performed is because the logic in `functionCallsIterator` needs
  -- the type witnesses to already be present in the function signatures and thus in the scope of the function
  iterateWithOthers functionCallsIterator roots''
  where
    -- Iterator to rewrite top levels
    topLevelIterator :: Root -> [Root] -> State Int Root
    topLevelIterator (RModule rModule@Module {moduleTopLevels}) _ = do
      mappedTL <- mapM rewriteTopLevel moduleTopLevels
      return $ RModule $ rModule {moduleTopLevels = mappedTL}
    topLevelIterator (RScript rScript@Script {scriptTopLevels}) _ = do
      mappedTL <- mapM rewriteTopLevel scriptTopLevels
      return $ RScript $ rScript {scriptTopLevels = mappedTL}

    -- Iterator to rewrite function calls
    functionCallsIterator :: Root -> [Root] -> State Int Root
    functionCallsIterator root otherRoots = return $ fst $ traverseRootPostOrder rewriteFunctionCalls traversalIdentity root () otherRoots

    -- Helper function, used to rewrite a single type argument to a type witness
    mapTArgToFArg :: Type -> Set.Set Identifier -> IntermediateTypeWitnessExpr
    mapTArgToFArg (TypeConstructor (LocalNameAccessChain tCons) tArgs) tParams
      -- If the type constructor is one of the type parameters, rewrite it as its identifier
      -- Example `T`
      | Set.member tCons tParams = IntermediateTypeWitnessIdent tCons
      -- Otherwise, it is a struct or stdlib type and might also have type args itself
      -- Example `MyStruct`, `u64` or `MyStruct<...>`
      | otherwise = IntermediateTypeWithnessC (LocalNameAccessChain tCons) $ map (`mapTArgToFArg` tParams) tArgs
    -- Any non-local type constructor is surely not a type parameter
    mapTArgToFArg (TypeConstructor tCons tArgs) tParams =
      IntermediateTypeWithnessC tCons $ map (`mapTArgToFArg` tParams) tArgs
    mapTArgToFArg t _ = error $ "Unexpected type argument: " ++ show t

    -- Given the current scopes, returns all the identifiers corresponding to type witnesses
    extractTypeWitnessesFromScopes :: [Scope] -> Set.Set Identifier
    extractTypeWitnessesFromScopes [] = Set.empty
    extractTypeWitnessesFromScopes (sc : scs) = extractFromScope sc `Set.union` extractTypeWitnessesFromScopes scs
      where
        -- Inner helper function that given a single scope, extracts all the identifiers corresponding to type witnesses
        -- Note that type witnesses can only be local identifiers
        extractFromScope :: Scope -> Set.Set Identifier
        extractFromScope scope = Set.fromList [ident | (LocalNameAccessChain ident, VariableAnnotations _ IntermediateTypeWitnessType) <- Map.toList scope]

    -- Rewrites function calls to add the type witnesses as function parameters
    rewriteFunctionCalls :: TraversalMapper Expr ()
    rewriteFunctionCalls expr@(PositionalStructExprOrFunctionCallExpr fc@PositionalStructExprOrFunctionCall {pseofcNameAccessChain, pseofcFields, pseofcTypeArgs}) scopes st =
      case getNacFromScopes pseofcNameAccessChain scopes of
        -- In this case, it is a function call and not a positional struct
        VariableAnnotations _ (TypeArrow _ _) ->
          let -- Always add the corresponding type witnesses
              -- NOTE that the identifiers of the type parameters are supplied to `extractTypeWitnessesFromScopes` by extracting the names of the type witnesses
              -- This relies to the fact that the name of the type parameter and the corresponding type witness must be identical, otherwise a type argument would not be
              -- recognized as coming from a type param
              -- The reason for doing this is that the traversal has no way to know the type parameters defined by the current function
              -- This reasoning requires that the type witnesses have already been inserted into the (caller) function signature
              pseofcFields' = pseofcFields ++ map (IntermediateExprExpr . IntermediateTypeWitnessExprExpr . (`mapTArgToFArg` extractTypeWitnessesFromScopes scopes)) pseofcTypeArgs
           in (PositionalStructExprOrFunctionCallExpr $ fc {pseofcFields = pseofcFields'}, st)
        -- Otherwise, leave the function call / positional struct unchanged
        _ -> (expr, st)
    rewriteFunctionCalls expr _scopes st = (expr, st)

    -- Maps any type parameter to a corresponding function parameter
    -- The function parameter will have the same identifier as the type param, but in snake_case
    mapTParamToFParam :: TypeParameter -> State Int Parameter
    mapTParamToFParam TypeParameter {typeIdentifier} = do
      currUUID <- get
      put $ currUUID + 1
      return $ Parameter {parameterIdentifier = toSnake typeIdentifier, parameterUUID = Just currUUID, parameterType = IntermediateTypeWitnessType}

    -- Used to rewrite the occurrencies of type parameters inside the body of the function
    -- Accepts as input a map consisting in all the original names of the type parameters associated
    -- with the respective snake_case version
    --
    -- Leaves unaltered any non-type parameter occurrence
    helperTrTypes :: Map.Map Identifier Identifier -> Type -> Type
    helperTrTypes renames (TypeConstructor (LocalNameAccessChain tCons) tArgs) =
      TypeConstructor (LocalNameAccessChain $ fromMaybe tCons $ Map.lookup tCons renames) tArgs
    helperTrTypes _ t = t

    -- Given the map of renamed type parameter identifiers and a type param to renamed, ensures it is present and returns the renamed version
    assertRenamed :: Map.Map Identifier Identifier -> TypeParameter -> TypeParameter
    assertRenamed renames tp@TypeParameter {typeIdentifier} =
      case Map.lookup typeIdentifier renames of
        Nothing -> error $ "Expected a type parameter to have been renamed: " ++ show tp
        Just typeIdentifier' -> tp {typeIdentifier = typeIdentifier'}

    -- Rewrites any top level definition
    --
    -- Note how type parameters are always kept in the rewritten function (but in snake_case),
    -- so to still be able to identify if a function is polymorphic or not (used when rewriting function calls).
    -- Additionally, it is needed when downcasting resources extracted from the global storage
    rewriteTopLevel :: TopLevel -> State Int TopLevel
    rewriteTopLevel (TopLevelFunction func@Function {functionTypeParameters}) = do
      -- First, map each type parameter to the corresponding snake_case version
      let renames :: Map.Map Identifier Identifier = Map.fromList $ map (\tp -> (typeIdentifier tp, toSnake $ typeIdentifier tp)) functionTypeParameters

      -- Then, create the new function parameters
      newParams <- mapM mapTParamToFParam functionTypeParameters
      -- Also remap the type parameters since the transformBi does not apply to them
      let functionTypeParameters' = map (assertRenamed renames) functionTypeParameters

      -- `func'` contains the updated parameters and type parameters
      let func' =
            func
              { functionTypeParameters = functionTypeParameters',
                functionParameters = functionParameters func ++ newParams
              }

      -- Rename every type occurrence in the function, including types in the function signature
      -- Passing the map allows to know which type comes from a type parameter
      let func'' = transformBi (helperTrTypes renames) func'

      -- Note: the rewriting of function calls has been removed and has already been performed in `rewriteFunctionCalls`
      return $ TopLevelFunction func''

    -- Also named and positional structs should be rewritten
    -- not with type witnesses by just snake_case-ing their type arguments
    rewriteTopLevel (TopLevelNamedStruct ns@NamedStruct {namedStructTypeParameters, namedStructFields}) =
      let renames = Map.fromList $ map (\tp -> (typeIdentifier tp, toSnake $ typeIdentifier tp)) namedStructTypeParameters
          namedStructTypeParameters' = map (assertRenamed renames) namedStructTypeParameters
          namedStructFields' = transformBi (helperTrTypes renames) namedStructFields
       in return $
            TopLevelNamedStruct $
              ns
                { namedStructTypeParameters = namedStructTypeParameters',
                  namedStructFields = namedStructFields'
                }
    rewriteTopLevel (TopLevelPositionalStruct ps@PositionalStruct {positionalStructTypeParameters, positionalStructFields}) =
      let renames = Map.fromList $ map (\tp -> (typeIdentifier tp, toSnake $ typeIdentifier tp)) positionalStructTypeParameters
          positionalStructTypeParameters' = map (assertRenamed renames) positionalStructTypeParameters
          positionalStructFields' = transformBi (helperTrTypes renames) positionalStructFields
       in return $
            TopLevelPositionalStruct $
              ps
                { positionalStructTypeParameters = positionalStructTypeParameters',
                  positionalStructFields = positionalStructFields'
                }
    -- Otherwise, leaves the top level unchanged
    rewriteTopLevel tl = return tl

-- |
-- Given an identifier, returns its snake_case version, prepended by a `tw_` to avoid name clashes.
--
-- Used by type parameters since must be lowercase in Aiken
toSnake :: Identifier -> Identifier
toSnake (Identifier ident) = Identifier $ "tw_" ++ reverse (foldl helper "" ident)
  where
    -- Helper for the foldl, creates the snake_case version but reversed
    helper :: String -> Char -> String
    helper acc ch =
      case (isUpper ch, null acc) of
        (True, True) ->
          -- If the first letter is in uppercase, just lower it
          toLower ch : acc
        (True, False) ->
          -- If any non-first letter is in uppercase, prepend an underscore in front of its lowercase version
          -- (Reversed since foldl)
          toLower ch : '_' : acc
        -- Otherwise do nothing
        (False, _) -> ch : acc