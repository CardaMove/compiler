module Move.Translations.Polymorphism (translatePolymorphismInRoot) where

import Control.Monad.State (MonadState (get, put), State)
import Data.Foldable (toList)
import Data.Generics.Uniplate.Data (transformBi)
import Data.Sequence (fromList, mapWithIndex)
import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (TraversalMapper, traversalIdentity, traverseRootPostOrder)
import Move.Translations.Utils (VariableAnnotations (..), getNacFromScopes, inferExprType, isTypeParametric, iterateWithOthers)

-- |
-- Given an AST with parametric polymorphism, rewrites it by removing generics and replacing them via
-- an intermediate supertype `IntermediateTypeData`, adding downcasts or upcasts when needed
translatePolymorphismInRoot :: [Root] -> State AnnotatedUUID [Root]
translatePolymorphismInRoot roots = do
  -- First, introduce the required downcasts or upcasts when needed
  roots' <- iterateWithOthers castingIterator roots

  -- Then, replace each occurrence of type parameter with the intermediate supertype
  -- Note that for this passage there is no need to consider other roots when iterating,
  -- since it just performs substitution of some nodes
  let root'' = map tParamsToDataIterator roots'
  return root''
  where
    -- Iterator to add the necessary castings
    castingIterator :: Root -> [Root] -> State AnnotatedUUID Root
    castingIterator root otherRoots = do
      currUUID <- get
      let (root', updatedUUID) = traverseRootPostOrder insertCasting traversalIdentity root currUUID otherRoots
      put updatedUUID
      return root'

    -- Iterator to replace type parameters with intermediate supertype
    tParamsToDataIterator :: Root -> Root
    tParamsToDataIterator (RModule rModule@Module {moduleTopLevels}) =
      let mappedTL = map replaceTParamsWithData moduleTopLevels
       in RModule $ rModule {moduleTopLevels = mappedTL}
    tParamsToDataIterator (RScript rScript@Script {scriptTopLevels}) =
      let mappedTL = map replaceTParamsWithData scriptTopLevels
       in RScript $ rScript {scriptTopLevels = mappedTL}

    -- Given a function declaration, rewrites each occurrence of type parameters with the intermediate supertype `IntermediateTypeData`
    replaceTParamsWithData :: TopLevel -> TopLevel
    replaceTParamsWithData tlf@(TopLevelFunction Function {functionTypeParameters}) =
      let tParamIdents = Set.fromList $ map typeIdentifier functionTypeParameters
       in transformBi (rewriteTypeAsData tParamIdents) tlf
    replaceTParamsWithData tl = tl

-- |
-- Handles casting any expression from one type to another
-- The main problem is that casting between types that accept type arguments are not supported, for example:
--
-- Downcasting A<Data> into A<concreteType> is not allowed <------ !!!
-- The workaround is to wrap the right value with an `as_data`
-- `expect b: A<Int> = A{b: as_data(12)}`
--
-- Upcasting A<concreteType> into A<Data> is not allowed <------ !!!
-- The workaround is to wrap the right value with an `as_data`
-- `expect b: A<Data> = A{b: 12}`
--
-- Additionally, functions return the CPS state, which is an opaque type due to the use of Dict, so it can not be downcasted from Data
--
-- If the expression returns a tuple, each element of the tuple needs to be handled individually, for example to avoid the CPS to be upcasted to Data
--
-- So, first unwrap the tuple and then cast each element individually
-- For example, suppose `myFun<t>(...): (MyType<t>, CPS)` invoked with `u64` as type argument will be translated as:
--
-- ```
-- let (el_0, el_1): (MyType<Data>, CPS) = myFun<u64>(...);
-- (
--  {cast as_data(el_0) to MyType<Int>},
--  el_1 // since CPS
-- )
-- ```
castExpr :: Expr -> Type -> Type -> AnnotatedUUID -> Expr
castExpr expr (TypeTuple tsFrom) (TypeTuple tsTo) currUUID =
  SequenceExpr $
    Sequence
      { sequenceUses = [],
        sequenceItems =
          -- An initial `let` bind to unwrap the tuple
          -- Each i-th variable `el_i` is associated to the initial type of that tuple
          -- TODO: Maybe add a UUID to each variable
          [ SequenceItemBindExpr $
              Bindings
                { bindings = BindedTuple $ toList $ mapWithIndex (\idx _ -> BindIdentifier (Identifier $ "el_" ++ show idx) Nothing) $ fromList tsFrom,
                  bindingsBindType = Just $ TypeTuple tsFrom,
                  bindingsBindExpr = Just expr
                }
          ],
        -- Then, returns a new tuple, where each element is casted to the correct type
        -- However, the CPS is the only type that must not be casted, since it is opaque and would result in a compile error
        sequenceEndExpr =
          Just
            $ CommaExpr
            $ toList
            $ mapWithIndex
              ( \idx (tData, t) ->
                  case t of
                    -- Do not cast the CPS
                    IntermediateTypeScopes -> NameAccessChainExpr $ LocalNameAccessChain $ Identifier $ "el_" ++ show idx
                    -- Recursively cast any other type
                    _ -> castExpr (NameAccessChainExpr $ LocalNameAccessChain $ Identifier $ "el_" ++ show idx) tData t currUUID
              )
            $ fromList
            $ zip tsFrom tsTo
      }
-- It is not possible that the initial and final types have different arity
castExpr expr (TypeTuple _) _ _ = error $ "Mismatching function return type parametric and not: " ++ show expr
castExpr expr _ (TypeTuple _) _ = error $ "Mismatching function return type parametric and not: " ++ show expr
-- If the type is not a tuple, simply cast it
castExpr expr _ t currUUID =
  CastingTerm $
    Casting
      { castingExpr = IntermediateExprExpr $ IntermediateAsData expr,
        castingType = t
      }

-- |
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
-- TODO: return an updated current UUID
insertCasting :: TraversalMapper Expr AnnotatedUUID
insertCasting expr@(PositionalStructExprOrFunctionCallExpr fc@PositionalStructExprOrFunctionCall {pseofcNameAccessChain, pseofcFields}) scopes currUUID =
  case getNacFromScopes pseofcNameAccessChain scopes of
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
              upcastFuncArg :: Expr -> Type -> Expr
              upcastFuncArg fArg tParam
                | isTypeParametric tParam tParamsSet = castExpr fArg (inferExprType fArg scopes) (transformBi (rewriteTypeAsData tParamsSet) tParam) currUUID
              upcastFuncArg fArg _ = fArg

          fc' = fc {pseofcFields = pseofcFields'}

          -- If the return type is parametric on the type params defined on that function, wrap the returned value into a cast
          expr' =
            if not (null tParams) && isTypeParametric (last tFunc) tParamsSet
              -- Function calls always return a tuple since at least we have the CPS
              -- So, infer the final type and also check the type returned by the function (with Data already replaced)
              -- Then, forward the casting to the helper function
              then castExpr (PositionalStructExprOrFunctionCallExpr fc') (transformBi (rewriteTypeAsData tParamsSet) (last tFunc)) (inferExprType expr scopes) currUUID
              else
                PositionalStructExprOrFunctionCallExpr fc'
       in (expr', currUUID)
    -- Otherwise, leave the function call / positional struct unchanged
    _ -> (expr, currUUID)
insertCasting expr _scopes st = (expr, st)

-- |
-- Helper function for `replaceTParamsWithData`
-- Given the set of type parameter names and a type, replaces all the occurrences of the type parameter names with the intermediate supertype
-- Note that this function is intended to be used inside a `transformBi`
rewriteTypeAsData :: Set.Set Identifier -> Type -> Type
rewriteTypeAsData tParamIdents (TypeConstructor (LocalNameAccessChain tCons) [])
  | Set.member tCons tParamIdents = IntermediateTypeData
rewriteTypeAsData tParamIdents t@(TypeConstructor (LocalNameAccessChain tCons) _)
  | Set.member tCons tParamIdents = error $ "Found a parametric type constructor with non-empty type arguments: " ++ show t
rewriteTypeAsData _ t = t