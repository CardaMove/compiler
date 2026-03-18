module Move.Translations.Polymorphism (translatePolymorphismInRoot) where

import Data.Generics.Uniplate.Data (transformBi)
import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (VariableAnnotations (..), getIdentifierFromScopes, inferExprType, isTypeParametric)

-- |
-- Given an AST with parametric polymorphism, rewrites it by removing generics and replacing them via
-- an intermediate supertype `IntermediateTypeData`, adding downcasts or upcasts when needed
translatePolymorphismInRoot :: Root -> Root
translatePolymorphismInRoot root =
  let --
      -- First, introduce the required downcasts or upcasts when needed
      root' = fst $ traverseRootPostOrder insertCasting traversalIdentity root ()

      -- Then, replace each occurrence of type parameter with the intermediate supertype
      root'' = case root' of
        RModule rModule@Module {moduleTopLevels} ->
          let mappedTL = map replaceTParamsWithData moduleTopLevels
           in RModule $ rModule {moduleTopLevels = mappedTL}
        RScript rScript@Script {scriptTopLevels} ->
          let mappedTL = map replaceTParamsWithData scriptTopLevels
           in RScript $ rScript {scriptTopLevels = mappedTL}
   in root''
  where
    -- Helper function for `replaceTParamsWithData`
    -- Given the set of type parameter names and a type, replaces all the occurrences of the type parameter names with the intermediate supertype
    -- Note that this function is intended to be used inside a `transformBi`
    rewriteTypeAsData :: Set.Set Identifier -> Type -> Type
    rewriteTypeAsData tParamIdents (TypeConstructor (LocalNameAccessChain tCons) [])
      | Set.member tCons tParamIdents = IntermediateTypeData
    rewriteTypeAsData tParamIdents t@(TypeConstructor (LocalNameAccessChain tCons) _)
      | Set.member tCons tParamIdents = error $ "Found a parametric type constructor with non-empty type arguments: " ++ show t
    rewriteTypeAsData _ t = t

    -- Given a function declaration, rewrites each occurrence of type parameters with the intermediate supertype `IntermediateTypeData`
    replaceTParamsWithData :: TopLevel -> TopLevel
    replaceTParamsWithData tlf@(TopLevelFunction Function {functionTypeParameters}) =
      let tParamIdents = Set.fromList $ map typeIdentifier functionTypeParameters
       in transformBi (rewriteTypeAsData tParamIdents) tlf
    replaceTParamsWithData tl = tl

    --
    -- Traversal expression that rewrites each function call in the following way:
    --
    --  - The function return is wrapped into a downcasting to the resolved type (only if parametric)
    --  - Each function argument is upcasted if the parameter has a parametric type
    --
    -- The (down)casting is needed only when the return type is parametric on some type parameters of the called function
    -- This is because the caller function would thus (probably) resolve that parametric type into a concrete type, and in Aiken this must be done via an `expect`
    --
    -- Example: `my_func<a>(...): MyStruct<a>` or `my_func<a>(...): a` would (probably) be resolved as concrete types:
    --
    -- `let res: MyStruct<u64> = my_func<u64>(...)`
    --
    -- Note: there is the possibility that the type of the `res` variable is still parametric on `a` (type parameter forwarding?),
    -- in this case the casting is not needed, but in any case it does not impact runtime exectution
    --
    -- Similarly, it also performs an (up)casting when function arguments are passed to function parameters that are themselves type-parametric
    insertCasting :: TraversalMapper Expr ()
    insertCasting expr@(PositionalStructExprOrFunctionCallExpr fc@PositionalStructExprOrFunctionCall {pseofcNameAccessChain = LocalNameAccessChain funcIdent, pseofcFields}) scopes st =
      case getIdentifierFromScopes funcIdent scopes of
        -- In this case, it is a function call and not a positional struct
        VariableAnnotations _ (TypeArrow tParams tFunc) ->
          let tParamsSet = Set.fromList tParams
              -- Upcast any function argument if the corresponding func param is parametric
              -- Note that `tFunc` has one element more since it contains the return type,
              -- however the `zipWith` ignores it
              -- Here it is assumed that the number of arguments and function parameters match
              pseofcFields' = zipWith upcastFuncArg pseofcFields tFunc
                where
                  -- Given a function argument and the type of the corresponding function parameter, wraps the argument
                  -- into an (up)casting if the type is parametric on the type params defined on the called function
                  -- Note that the upcasted type is rewritten with the `IntermediateTypeData` when a parametric type occurs
                  -- For example, an argument passed to a parameter of type `MyType<tw_a>` is upcasted to `MyType<IntermediateTypeData>`
                  -- TODO: Handle the tuple separately (one cast for each element)
                  -- Update: maybe it is not needed, since they are cated to Data in any case
                  upcastFuncArg :: Expr -> Type -> Expr
                  upcastFuncArg fArg tParam
                    | isTypeParametric tParam tParamsSet =
                        CastingTerm $
                          Casting
                            { castingExpr = IntermediateExprExpr $ IntermediateAsData fArg,
                              castingType = transformBi (rewriteTypeAsData tParamsSet) tParam
                            }
                  upcastFuncArg fArg _ = fArg

              fc' = fc {pseofcFields = pseofcFields'}

              -- If the return type is parametric on the type params defined on that function, wrap the returned value into a cast
              -- TODO: Handle the tuple separately (one cast for each element)
              expr' =
                if not (null tParams) && isTypeParametric (last tFunc) tParamsSet
                  then
                    CastingTerm $
                      Casting
                        { castingExpr = IntermediateExprExpr $ IntermediateAsData $ PositionalStructExprOrFunctionCallExpr fc',
                          castingType = inferExprType expr scopes
                        }
                  else
                    PositionalStructExprOrFunctionCallExpr fc'
           in (expr', st)
        -- Otherwise, leave the function call / positional struct unchanged
        _ -> (expr, st)
    -- TODO: non local function calls
    insertCasting expr@(PositionalStructExprOrFunctionCallExpr _) _scopes _st = error $ "Non-local function calls currently not supported: " ++ show expr
    insertCasting expr _scopes st = (expr, st)