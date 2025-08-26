module Move.Translations.Loops (translateLoopsToWhile, translateWhilesToFunctions, translateWhilesToFunctionsInModule) where

-- Importing from Uniplate.Data allows to derive Biplate instances automatically from data types that derive Data

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Data.List (nub, sort)
import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (traverseExprPostOrder, traverseModulePostOrder)
import Move.Translations.Utils (Scope, getIdentifierTypeFromScope, getValueOrDefault, isIdentifierInScope, unknownType)

-- |
-- Translates all `loop expr` expressions into `while(true) expr`.
--
-- Works recursively on any node of the AST
translateLoopsToWhile :: (Data from) => from -> from
translateLoopsToWhile = transformBi f
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

-- |
-- Given a while loop, returns a corresponding function call along with function declaration.
--
-- The function name is passed as input to this function.
--
-- Free variables, meaning variables declared outside of the loop, are converteed to references passed as function parameters.
--
-- Control keywords such as continue and break are handled internally on the function.
--
-- The newly declared function will have as body a sequence with just an end expression, consisting in a if-then term.
--
-- The if-then term itself will be composed of a sequence, contaning the while body and a recursive call as end expression
mapWhileToFunction :: While -> [Scope] -> Identifier -> (PositionalStructExprOrFunctionCall, Function)
mapWhileToFunction (While {whileCondition, whileExpr}) scopes functionName =
  let -- Map both the condition expression and the body of the while
      (mappedConditionExpr, freeVarsInConditionExpr) = mapFreeVariablesToDerefsInExpr whileCondition
      (mappedBodyExpr, freeVarsInBodyExpr) = mapFreeVariablesToDerefsInExpr whileExpr -- TODO: Add translation for break and continue
      -- Get all the free variables with their type
      -- Note that variables are sorted so to avoid confusion when testing
      allFreeVars = sort $ nub (freeVarsInConditionExpr ++ freeVarsInBodyExpr)
      freeVarsWithType = map (\ident -> (ident, getIdentifierTypeFromScope ident scopes)) allFreeVars
      -- For each free variable, create a function parameter as a mutable reference
      functionParameters = map mapFreeVariableToFunctionParameter freeVarsWithType
      -- For the invocation from the original function, dereference the variables with the same name
      functionCallArguments = map (UnaryOpExpr . MutableReference . NameAccessChainExpr . LocalNameAccessChain) allFreeVars
      -- For the recursive invocation instead, just pass the variables without dereferencing since they are already defined as references
      recursiveFunctionCallArguments = map (NameAccessChainExpr . LocalNameAccessChain) allFreeVars

      functionBody =
        Just $
          Sequence
            { sequenceUses = [],
              sequenceItems = [],
              sequenceEndExpr =
                Just $
                  IfThenElseTerm $
                    IfThenElse
                      { ifThenElseCondition = mappedConditionExpr,
                        -- The if branch will consist in a sequence with two expression
                        -- The first expression is the while expression (Note that this might result in a sequence inside a sequence)
                        -- and the last (end) expression is the recursive call
                        -- Note that this does not interphere with any return value, since in any case the while returns unit
                        ifThenElseIfBranch =
                          SequenceExpr $
                            Sequence
                              { sequenceUses = [],
                                sequenceItems = [SequenceItemExpr mappedBodyExpr],
                                -- TODO: Add check if break not hit
                                sequenceEndExpr =
                                  Just $
                                    PositionalStructExprOrFunctionCallExpr $
                                      PositionalStructExprOrFunctionCall
                                        { pseofcNameAccessChain = LocalNameAccessChain functionName,
                                          pseofcTypeArgs = [],
                                          pseofcFields = recursiveFunctionCallArguments
                                        }
                              },
                        ifThenElseElseBranch = Nothing
                      }
            }
   in ( PositionalStructExprOrFunctionCall
          { pseofcNameAccessChain = LocalNameAccessChain functionName,
            pseofcTypeArgs = [],
            pseofcFields = functionCallArguments
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
traversalHelper :: (Expr -> [Scope] -> ([Function], Set.Set Expr) -> (Expr, ([Function], Set.Set Expr)))
--  When a while loop is found, translate it
traversalHelper (WhileTerm whileExpr) scopes' (functionDecls, exprsWithBreak) =
  let (functionCall, functionDecl) = mapWhileToFunction whileExpr scopes' (Identifier $ "mapped_while_" ++ show (length functionDecls))
   in (PositionalStructExprOrFunctionCallExpr functionCall, (functionDecl : functionDecls, exprsWithBreak))
--  When a break is found
traversalHelper Break _ (functionDecls, exprsWithBreak) =
  -- Replace it with an assignment `break_hit = true`
  let expr' =
        AssignmentExpr $
          Assignment
            { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "break_hit",
              assignmentRight = ValueLiteral $ Boolean True
            }
   in -- Add the new assignment to the set of expressions that contain a break
      -- (in fact, this assignment will act as a break)
      (expr', (functionDecls, Set.insert expr' exprsWithBreak))
-- When a SequenceExpr is found
traversalHelper currSequence@(SequenceExpr Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) _ (functionDecls, exprsWithBreak) =
  -- Check if any of the inner sequence items contain a break. If so, all the subsequent items need to be inserted inside an if
  --
  -- Start by converting the end expression into a sequence item.
  -- Note that this does not interphere with any return value, since in any case the while returns unit
  let endExprAsSequenceItem = case sequenceEndExpr of
        Nothing -> []
        Just expr -> [SequenceItemExpr expr]

      -- Now, if a sequence item contains a break, replace all the subsequent items with an if
      (mappedSequenceItems, exprsWithBreak') = foldr mapSeqItem (endExprAsSequenceItem, exprsWithBreak) sequenceItems
        where
          mapSeqItem seqItemExpr (subseqItems, exprsWithBreakBefore) =
            -- Internal function that, given the expression inside a sequence item, returns the correct sequcne items and state depending if the current expression has a break inside or not
            let mapItem currExpr =
                  if Set.member currExpr exprsWithBreakBefore
                    then
                      let ifThenElse =
                            SequenceItemExpr $
                              IfThenElseTerm $
                                IfThenElse
                                  { ifThenElseCondition = UnaryOpExpr $ Negation $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "break_hit",
                                    ifThenElseIfBranch =
                                      SequenceExpr $
                                        Sequence
                                          { sequenceUses = [],
                                            sequenceItems = subseqItems,
                                            sequenceEndExpr = Nothing
                                          },
                                    ifThenElseElseBranch = Nothing
                                  }
                          -- Remove the current expression from the set of expression that have a break, both for cleaning and to mark this expression as "handled"
                          -- Then, add the whole sequence expression to this set. This operation might be redundant if the sequence ha already been added
                          exprsWithBreakAfter = Set.insert currSequence $ Set.delete currExpr exprsWithBreakBefore
                       in ([seqItemExpr, ifThenElse], exprsWithBreakAfter)
                    else (seqItemExpr : subseqItems, exprsWithBreakBefore)
             in -- The action to take is the same independently if the sequence item is an expression or a binding
                case seqItemExpr of
                  SequenceItemExpr itemExpr -> mapItem itemExpr
                  SequenceItemBindExpr Bindings {bindingsBindExpr = Just bindingsBindExpr} -> mapItem bindingsBindExpr
                  _ -> (seqItemExpr : subseqItems, exprsWithBreakBefore)
   in -- Return a new sequence.
      -- Note that the ending expression is always moved as a sequence item, unregarding if breaks are encountered or not
      -- Note that this does not interphere with any return value, since in any case the while returns unit
      --
      -- No additional functions should be declared, the set of expressions containing a break might have been updated
      ( SequenceExpr $
          Sequence
            { sequenceUses = sequenceUses,
              sequenceItems = mappedSequenceItems,
              sequenceEndExpr = Nothing
            },
        (functionDecls, exprsWithBreak')
      )
--  Otherwise, do nothing
traversalHelper expr' _ state = (expr', state)

-- |
-- Given an expression, recursively converts each while loop into a function declaration,
-- and substitutes the loop expression with that function call
-- Also see documentation of `mapWhileToFunction`
translateWhilesToFunctions :: Expr -> [Scope] -> (Expr, [Function])
translateWhilesToFunctions expr scopes =
  let (expr', (functionDecls, _)) = traverseExprPostOrder traversalHelper expr scopes ([], Set.empty)
   in (expr', functionDecls)

-- |
-- Given a module, translates all while loops into function calls.
--
-- The corresponding function declarations are added inside the module
-- Also see documentation of `mapWhileToFunction`
translateWhilesToFunctionsInModule :: Module -> Module
translateWhilesToFunctionsInModule currModule =
  let (translatedModule@Module {moduleTopLevels}, (functionDecls, _)) = traverseModulePostOrder traversalHelper currModule ([], Set.empty)
      newTopLevels = map TopLevelFunction functionDecls
   in translatedModule {moduleTopLevels = newTopLevels ++ moduleTopLevels}
