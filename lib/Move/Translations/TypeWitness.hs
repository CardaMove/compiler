module Move.Translations.TypeWitness where

import Control.Monad.State
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), getIdentifierFromScopes, isIdentifierInScope)

-- |
-- Given an AST, rewrites all function declarations to add parameters for type witnesses
rewriteTopLevelsInRoot :: Root -> State Int Root
rewriteTopLevelsInRoot root = do
  case root of
    RModule rModule@Module {moduleTopLevels} -> do
      mappedTL <- mapM rewriteTopLevel moduleTopLevels
      return $ RModule $ rModule {moduleTopLevels = mappedTL}
    RScript rScript@Script {scriptTopLevels} -> do
      mappedTL <- mapM rewriteTopLevel scriptTopLevels
      return $ RScript $ rScript {scriptTopLevels = mappedTL}
  where
    -- Maps any type parameter to a corresponding function parameter
    mapTParamToFParam :: TypeParameter -> State Int Parameter
    mapTParamToFParam TypeParameter {typeIdentifier} = do
      currUUID <- get
      put $ currUUID + 1
      return $ Parameter {parameterIdentifier = typeIdentifier, parameterUUID = Just currUUID, parameterType = IntermediateTypeWitnessType}

    -- Rewrites any top level, particularly function definitions
    --
    -- Note how type parameters are always kept in the rewritten function,
    -- so to still be able to identify if a function is polymorphic or not (used when rewriting function calls).
    -- Additionally, it is needed when downcasting resources extracted from the global storage
    rewriteTopLevel :: TopLevel -> State Int TopLevel
    rewriteTopLevel (TopLevelFunction func@Function {functionTypeParameters, functionParameters}) = do
      newParams <- mapM mapTParamToFParam functionTypeParameters
      return $
        TopLevelFunction $
          func {functionParameters = functionParameters ++ newParams}
    -- Otherwise, leaves the top level unchanged
    rewriteTopLevel tl = return tl

-- |
-- Given an AST, rewrites all type parameters in function declarations as an additional
-- function parameter acting as type witness and adds the corresponding arguments to function calls.
translateTParamsInRoot :: Root -> State Int Root
translateTParamsInRoot root = do
  -- First, rewrite the function declarations to add the type witness parameters
  translatedRoot <- rewriteTopLevelsInRoot root

  -- Then, rewrite all function calls inside the AST
  let translatedRoot' = fst $ traverseRootPostOrder translateFunctionCall traversalIdentity translatedRoot ()
  return translatedRoot'
  where
    -- FIXME: Maps any type arguments to a corresponding function argument
    --
    -- Note that only type constructors can be used as type arguments. References and tuples are not allowed
    --
    -- TODO: add support for non local name access chain
    mapTArgToTypeWitness :: Type -> [Scope] -> IntermediateTypeWitnessExpr
    mapTArgToTypeWitness t@(TypeConstructor (LocalNameAccessChain tCons) tArgs) scopes =
      -- The type arguments might be a stdlib type such as `u64`
      -- In this case it would not be present in the scope
      if isIdentifierInScope tCons scopes
        then case getIdentifierFromScopes tCons scopes of
          -- In this case, the type corresponds to a struct, that might have inner type parameters
          VariableAnnotations _ (IntermediateTypeNamedStructDeclaration structTParams _) ->
            if length tArgs /= length structTParams
              then
                error $ "Mismatching number of type parameters and type arguments: " ++ show t
              else IntermediateTypeWithnessC (LocalNameAccessChain tCons) $ map (`mapTArgToTypeWitness` scopes) tArgs
          -- Otherwise, it must be a type parameter itself, with no type arguments
          --
          -- (meaning that T<u64> is forbidden for example)
          VariableAnnotations _ IntermediateTypeWitnessType -> IntermediateTypeWitnessIdent tCons
          _ -> error $ "Unexpected type constructor used as type argument: " ++ show tCons
        else
          -- If the type comes from the stdlib, the type witness is just the type itsel
          IntermediateTypeWithnessC (LocalNameAccessChain tCons) []
    mapTArgToTypeWitness t _ = error $ "Unexpected type argument: " ++ show t

    -- Translates a function call by adding the corresponding type witnesses, if any.
    --
    -- TODO: Note that it also handles calls to the global storage operators (not as IR nodes)
    --
    -- TODO: Handle non local name access chains and inferring type arguments when unspecified
    translateFunctionCall :: TraversalMapper Expr ()
    translateFunctionCall (PositionalStructExprOrFunctionCallExpr func@PositionalStructExprOrFunctionCall {pseofcNameAccessChain = LocalNameAccessChain funcIdent, pseofcTypeArgs, pseofcFields}) scopes state' =
      case getIdentifierFromScopes funcIdent scopes of
        -- In this case, it is a function call and not a positional struct
        -- If the function is not polymorphic, leave it as is
        VariableAnnotations _ (TypeArrow [] _) -> (PositionalStructExprOrFunctionCallExpr func, state')
        -- Otherwise, rewrite it
        VariableAnnotations _ (TypeArrow tParams _) ->
          if length pseofcTypeArgs == length tParams
            then
              let newArgs = map (IntermediateExprExpr . IntermediateTypeWitnessExprExpr . (`mapTArgToTypeWitness` scopes)) pseofcTypeArgs
               in -- Append the new arguments to the function call
                  (PositionalStructExprOrFunctionCallExpr func {pseofcFields = pseofcFields ++ newArgs}, state')
            -- TODO: inferring type arguments when unspecified
            else error $ "Inferring type arguments is currently not supported, or mismatching number of type arguments: " ++ show func ++ ". Expected: " ++ show tParams
        _ -> (PositionalStructExprOrFunctionCallExpr func, state')
    translateFunctionCall expr _ state' = (expr, state')