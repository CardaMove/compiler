module Move.Translations.Loops (mapLoopsToWhile, translateWhilesToFunctions, translateWhilesToFunctionsInModule) where

-- Importing from Uniplate.Data allows to derive Biplate instances automatically from data types that derive Data

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Move.AST
import Move.Translations.TraversalUtils (traverseExprPostOrder, traverseModulePostOrder)
import Move.Translations.Utils (Scope, getIdentifierTypeFromScope, getValueOrDefault, isIdentifierInScope, unknownType)

-- |
-- Translates all `loop expr` expressions into `while(true) expr`.
--
-- Works recursively on any node of the AST
mapLoopsToWhile :: (Data from) => from -> from
mapLoopsToWhile = transformBi f
  where
    f (Loop expr) =
      WhileTerm $
        While
          { whileCondition = ValueLiteral $ Boolean True,
            whileExpr = expr
          }
    f expr = expr

-- |
-- Given an expression tree, maps all free variables in it, meaning the variables that are not declared in this scope or inner scopes,
-- with a corresponding dereference, and returns both the updated tree and the modified free variables
mapFreeVariablesToDerefsInExpr :: Expr -> (Expr, [Identifier])
mapFreeVariablesToDerefsInExpr expr = traverseExprPostOrder f expr [] []
  where
    f expr'@(NameAccessChainExpr (LocalNameAccessChain ident)) scopes state =
      if isIdentifierInScope ident scopes
        then (expr', state)
        else (UnaryOpExpr $ Dereference expr', ident : state)
    f expr' _ state = (expr', state)

-- |
-- Given a free variable, create a corresponding function parameter with the same identifier.
--
-- The parameter will be a mutable or immutable reference depending if the input variable is mutable or not.
--
-- If the type of the variable is known, it will be used as type argument for the reference, otherwise a dummy type will be returned
mapFreeVariableToFunctionParameter :: (Identifier, Maybe Type) -> Parameter
mapFreeVariableToFunctionParameter (ident, identType) =
  Parameter
    { parameterIdentifier = ident,
      parameterType = TypeMutableRef $ getValueOrDefault identType unknownType
    }

mapFreeVariableToFunctionArgument :: Identifier -> Expr
mapFreeVariableToFunctionArgument ident = UnaryOpExpr $ MutableReference $ NameAccessChainExpr $ LocalNameAccessChain ident

-- |
-- Given a while loop, returns a corresponding function call along with function declaration.
--
-- The function name is passed as input to this function.
--
-- Free variables, meaning variables declared outside of the loop, are converteed to references passed as function parameters.
--
-- Control keywords such as continue and break are handled internally on the function.
mapWhileToFunction :: While -> [Scope] -> Identifier -> (PositionalStructExprOrFunctionCall, Function)
mapWhileToFunction (While {whileCondition, whileExpr}) scopes functionName =
  let (mappedConditionExpr, freeVarsInConditionExpr) = mapFreeVariablesToDerefsInExpr whileCondition
      (mappedBodyExpr, freeVarsInBodyExpr) = mapFreeVariablesToDerefsInExpr whileExpr -- TODO: Add translation for break and continue
      freeVarsWithType = map (\ident -> (ident, getIdentifierTypeFromScope ident scopes)) (freeVarsInConditionExpr ++ freeVarsInBodyExpr)
      functionParameters = map mapFreeVariableToFunctionParameter freeVarsWithType
      functionArguments = map mapFreeVariableToFunctionArgument (freeVarsInConditionExpr ++ freeVarsInBodyExpr)

      functionBody =
        Just $
          Sequence
            { sequenceUses = [],
              sequenceItems =
                [ SequenceItemExpr $
                    IfThenElseTerm $
                      IfThenElse
                        { ifThenElseCondition = mappedConditionExpr,
                          ifThenElseIfBranch = mappedBodyExpr,
                          ifThenElseElseBranch = Nothing
                        }
                ],
              sequenceEndExpr = Nothing
            }
   in ( PositionalStructExprOrFunctionCall
          { pseofcNameAccessChain = LocalNameAccessChain functionName,
            pseofcTypeArgs = [],
            pseofcFields = functionArguments
          },
        Function
          { functionHasNativeModifier = False,
            functionVisibilityModifier = Nothing,
            functionHasEntryModifier = False,
            functionName = functionName,
            functionTypeParameters = [],
            functionParameters,
            functionReturnType = Nothing,
            functionAcquires = [],
            functionBody
          }
      )

-- |
-- Helper function for both `translateWhilesToFunctions` and `translateWhilesToFunctionsInModule`.
--
-- What it dos is calling the other `mapWhileToFunction` to translate a single while loop,
-- and ignoring any other expression type
translationHelper :: (Expr -> [Scope] -> [Function] -> (Expr, [Function]))
translationHelper (WhileTerm whileExpr) scopes' functionDecls =
  let (functionCall, functionDecl) = mapWhileToFunction whileExpr scopes' (Identifier $ "mapped_while_" ++ show (length functionDecls))
   in (PositionalStructExprOrFunctionCallExpr functionCall, functionDecl : functionDecls)
translationHelper expr' _ functionDecls = (expr', functionDecls)

-- |
-- Given an expression, recursively converts each while loop into a function declaration,
-- and substitutes the loop expression with that function call
translateWhilesToFunctions :: Expr -> [Scope] -> (Expr, [Function])
translateWhilesToFunctions expr scopes = traverseExprPostOrder translationHelper expr scopes []

-- |
-- Given a module, translates all while loops into function calls.
--
-- The corresponding function declarations are added inside the module
translateWhilesToFunctionsInModule :: Module -> Module
translateWhilesToFunctionsInModule currModule =
  let (translatedModule@Module {moduleTopLevels}, functionDecls) = traverseModulePostOrder translationHelper currModule []
      newTopLevels = map TopLevelFunction functionDecls
   in translatedModule {moduleTopLevels = newTopLevels ++ moduleTopLevels}
