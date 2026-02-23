module Move.Translations.TypeWitness (translateTParamsInRoot, toSnake) where

import Control.Monad.State
import Data.Char (toLower)
import Data.Generics.Uniplate.Data (transformBi, transformBiM)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import GHC.Unicode (isUpper)
import Move.AST

-- TODO: also rename types in structs (even if not type witness themselves)

-- |
-- Given an AST, rewrites all type parameters in function declarations as an additional
-- function parameter acting as type witness and adds the corresponding arguments to function calls.
--
-- Prepends to their name a `tw_` to avoid name clashes.
--
-- Additionally, rewrites all occurrences of type parameters in the body of the function.
translateTParamsInRoot :: Root -> State Int Root
translateTParamsInRoot root = do
  case root of
    RModule rModule@Module {moduleTopLevels} -> do
      mappedTL <- mapM rewriteTopLevel moduleTopLevels
      return $ RModule $ rModule {moduleTopLevels = mappedTL}
    RScript rScript@Script {scriptTopLevels} -> do
      mappedTL <- mapM rewriteTopLevel scriptTopLevels
      return $ RScript $ rScript {scriptTopLevels = mappedTL}
  where
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

    -- Used to add type witnesses to function calls
    -- Accepts as input the set of type parameters (already rewritten by `helperTrTypes`) of the function
    helperTrExpr :: Set.Set Identifier -> PositionalStructExprOrFunctionCall -> PositionalStructExprOrFunctionCall
    helperTrExpr tParams fc@PositionalStructExprOrFunctionCall {pseofcTypeArgs, pseofcFields} =
      fc
        { pseofcFields = pseofcFields ++ map (IntermediateExprExpr . IntermediateTypeWitnessExprExpr . mapTArgToFArg) pseofcTypeArgs
        }
      where
        -- Helper function, used to rewrite a single type argument to a type witness
        mapTArgToFArg :: Type -> IntermediateTypeWitnessExpr
        mapTArgToFArg (TypeConstructor (LocalNameAccessChain tCons) tArgs) =
          -- If the type constructor is one of the type parameters, rewrite it as its identifier
          -- Example `T`
          if Set.member tCons tParams
            then IntermediateTypeWitnessIdent tCons
            -- Otherwise, it is a struct or stdlib type and might also have type args itself
            -- Example `MyStruct`, `u64` or `MyStruct<...>`
            else IntermediateTypeWithnessC (LocalNameAccessChain tCons) $ map mapTArgToFArg tArgs
        -- Any non-local type constructor is surely not a type parameter
        mapTArgToFArg (TypeConstructor tCons tArgs) =
          IntermediateTypeWithnessC tCons $ map mapTArgToFArg tArgs
        mapTArgToFArg t = error $ "Unexpected type argument: " ++ show t

    -- Rewrites any top level
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
      let functionTypeParameters' = map (\tp -> tp {typeIdentifier = toSnake $ typeIdentifier tp}) functionTypeParameters

      -- `func'` contains the updated parameters and type parameters
      let func' =
            func
              { functionTypeParameters = functionTypeParameters',
                functionParameters = functionParameters func ++ newParams
              }

      -- Rename every type occurrence in the function, including types in the function signature
      -- Passing the map allows to know which type comes from a type parameter
      let func'' = transformBi (helperTrTypes renames) func'

      -- Then, for each function call add the corresponding type witness
      -- TODO: NOTE that since this step checks for the presence of type arguments rather than inspecting the type of the function
      -- looking for type params, it is not able to infer missing type arguments
      -- A fix/improvement could be: first, rewrite all function definitions, then, perform a traversal of each function and look at the signature of each called function
      let func''' = transformBi (helperTrExpr $ Set.fromList $ map typeIdentifier functionTypeParameters') func''

      return $ TopLevelFunction func'''

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
