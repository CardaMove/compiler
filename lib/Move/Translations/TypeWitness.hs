module Move.Translations.TypeWitness where

import Control.Monad.State
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), getIdentifierFromScopes)

-- |
-- Given an AST, rewrites all type parameters in function declarations as an additional
-- function parameter acting as type witness
translateTParamsInRoot :: Root -> State Int Root
translateTParamsInRoot root = do
  translatedRoot <- case root of
    RModule rModule@Module {moduleTopLevels} -> do
      mappedTL <- mapM rewriteTopLevel moduleTopLevels
      return $ RModule $ rModule {moduleTopLevels = mappedTL}
    RScript rScript@Script {scriptTopLevels} -> do
      mappedTL <- mapM rewriteTopLevel scriptTopLevels
      return $ RScript $ rScript {scriptTopLevels = mappedTL}

  let translatedRoot' = fst $ traverseRootPostOrder translateFunctionCall traversalIdentity translatedRoot ()
  return translatedRoot'
  where
    -- Maps any type parameter to a corresponding function parameter
    mapTParamToFParam :: TypeParameter -> State Int Parameter
    mapTParamToFParam TypeParameter {typeIdentifier} = do
      currUUID <- get
      put $ currUUID + 1
      return $ Parameter {parameterIdentifier = typeIdentifier, parameterUUID = Just currUUID, parameterType = IntermediateTypeWitnessType}

    -- Rewrites any top level, particularly function definitions
    rewriteTopLevel :: TopLevel -> State Int TopLevel
    rewriteTopLevel (TopLevelFunction func@Function {functionTypeParameters, functionParameters}) = do
      newParams <- mapM mapTParamToFParam functionTypeParameters
      return $
        TopLevelFunction $
          func
            { functionTypeParameters = [],
              functionParameters = functionParameters ++ newParams
            }
    -- Otherwise, leaves the top level unchanged
    rewriteTopLevel tl = return tl

    -- Maps any type arguments to a corresponding function argument
    --
    -- Note that only type constructors can be used as type arguments. References and tuples are not allowed
    --
    -- TODO: add support for non local name access chain
    mapTArgToTypeWitness :: Type -> [Scope] -> IntermediateTypeWitnessExpr
    mapTArgToTypeWitness t@(TypeConstructor (LocalNameAccessChain tCons) tArgs) scopes =
      case getIdentifierFromScopes tCons scopes of
        -- In this case, the type corresponds to a struct, that might have inner type parameters
        VariableAnnotations _ (IntermediateTypeNamedStructDeclaration structTParams _) ->
          if length tArgs /= length structTParams
            then
              error $ "Mismatching number of type parameters and type arguments: " ++ show t
            else IntermediateTypeWithnessC (LocalNameAccessChain tCons) $ map (`mapTArgToTypeWitness` scopes) tArgs
        -- Otherwise, it must be a type parameter itself, with no type arguments
        --
        -- (meaning that T<u64> is forbidden for example)
        VariableAnnotations _ IntermediateTypeWitnessType -> IntermediateTypeWithnessC (LocalNameAccessChain tCons) []
        _ -> error $ "Unexpected type constructor used as type argument: " ++ show tCons
    mapTArgToTypeWitness t _ = error $ "Unexpected type argument: " ++ show t

    -- Translates a function call by adding the corresponding type witnesses, if any
    --
    -- TODO: Handle non local name access chains and inferring type arguments when unspecified
    translateFunctionCall :: TraversalMapper Expr ()
    translateFunctionCall (PositionalStructExprOrFunctionCallExpr func@PositionalStructExprOrFunctionCall {pseofcNameAccessChain = LocalNameAccessChain funcIdent, pseofcTypeArgs, pseofcFields}) scopes state' =
      case getIdentifierFromScopes funcIdent scopes of
        -- In this case, it is a function call and not a positional struct
        VariableAnnotations _ (TypeArrow tParams _) ->
          if not $ null pseofcTypeArgs
            then
              -- In this case, the type arguments are present explicitly
              if length pseofcTypeArgs /= length tParams
                then error $ "Mismatching number of type parameters and type arguments in function call: " ++ show func
                else
                  let newArgs = map (IntermediateExprExpr . IntermediateTypeWitnessExprExpr . (`mapTArgToTypeWitness` scopes)) pseofcTypeArgs
                   in -- Append the new arguments to the function call
                      (PositionalStructExprOrFunctionCallExpr func {pseofcFields = pseofcFields ++ newArgs}, state')
            else error $ "Inferring type arguments is currently not supported: " ++ show func
        _ -> (PositionalStructExprOrFunctionCallExpr func, state')
    translateFunctionCall expr _ state' = (expr, state')