module Move.Translations.Loops (translateLoopsToWhile, translateWhilesToFunctions, translateWhilesToFunctionsInRoot) where

-- Importing from Uniplate.Data allows to derive Biplate instances automatically from data types that derive Data

import Data.Data (Data)
import Data.Generics.Uniplate.Data (children, transformBi)
import Data.List (nub, sort)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Move.AST
import Move.Translations.TraversalUtils (traverseExprPostOrder, traverseRootPostOrder, traversalBindingsIdentity)
import Move.Translations.Utils (Scope, VariableAnnotations (VariableAnnotations), booleanType, getIdentifierFromScopes, isIdentifierInScope)

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
mapFreeVariablesToDerefsInExpr expr = traverseExprPostOrder f traversalBindingsIdentity expr [] []
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
mapFreeVariableToFunctionParameter :: Identifier -> VariableAnnotations -> Parameter
mapFreeVariableToFunctionParameter ident (VariableAnnotations identUUID identType) =
  Parameter
    { parameterIdentifier = ident,
      parameterType = TypeMutableRef identType,
      parameterUUID = identUUID
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
--
-- Also see `traversalHelper`
mapWhileToFunction :: While -> [Scope] -> Bool -> Identifier -> (PositionalStructExprOrFunctionCall, Function)
mapWhileToFunction (While {whileCondition, whileExpr}) scopes containsBreak functionName =
  let -- Declaring flag variables
      breakHitDecl =
        SequenceItemBindExpr $
          Bindings
            { bindings = BindedSingle $ BindIdentifier (Identifier "break_hit") Nothing,
              bindingsBindType = Just booleanType,
              bindingsBindExpr = Just $ ValueLiteral $ Boolean False
            }
      continueHitDecl =
        SequenceItemBindExpr $
          Bindings
            { bindings = BindedSingle $ BindIdentifier (Identifier "continue_hit") Nothing,
              bindingsBindType = Just booleanType,
              bindingsBindExpr = Just $ ValueLiteral $ Boolean False
            }

      -- Map both the condition expression and the body of the while
      (mappedConditionExpr, freeVarsInConditionExpr) = mapFreeVariablesToDerefsInExpr whileCondition
      -- Translations for break and continue here have already been handled since it's a post order traversal
      -- It is needed to add the bindings for break (and continue) variables
      whileExprWithBreakBind =
        if containsBreak
          then case whileExpr of
            SequenceExpr currSeq@Sequence {sequenceItems} ->
              SequenceExpr $
                currSeq
                  { sequenceItems =
                      [breakHitDecl, continueHitDecl]
                        ++ sequenceItems
                  }
            -- Is it possible that the while body is not a sequence, but still has a break in it
            --    Example: `while(a > b) a = a + if (a < b) break else 2`
            -- In this case, wrap it in a sequence
            expr ->
              SequenceExpr $
                Sequence
                  { sequenceUses = [],
                    sequenceItems = [breakHitDecl, continueHitDecl],
                    sequenceEndExpr = Just expr
                  }
          else whileExpr

      (mappedBodyExpr, freeVarsInBodyExpr) = mapFreeVariablesToDerefsInExpr whileExprWithBreakBind
      --
      -- If a break_hit (or continue_hit) flag is added to the body of the while, it needs to be pushed on the scope so that its type can be retrieved
      -- Note: These new variables will not have any UUID
      scopes' =
        if containsBreak
          then Map.fromList [(Identifier "break_hit", VariableAnnotations Nothing booleanType), (Identifier "continue_hit", VariableAnnotations Nothing booleanType)] : scopes
          else scopes

      -- Get all the free variables with their type
      -- Note that variables are sorted so to avoid confusion when testing
      allFreeVars = sort $ nub (freeVarsInConditionExpr ++ freeVarsInBodyExpr)
      freeVarsWithType = map (\ident -> (ident, getIdentifierFromScopes ident scopes')) allFreeVars
      --
      -- For each free variable, create a function parameter as a mutable reference
      functionParameters = map (uncurry mapFreeVariableToFunctionParameter) freeVarsWithType
      -- For the invocation from the original function, dereference the variables with the same name
      functionCallArguments = map (UnaryOpExpr . MutableReference . NameAccessChainExpr . LocalNameAccessChain) allFreeVars
      --
      -- For the recursive invocation instead, just pass the variables without dereferencing since they are already defined as references
      recursiveFunctionCallArguments = map (NameAccessChainExpr . LocalNameAccessChain) allFreeVars
      recursiveFunctionCall =
        PositionalStructExprOrFunctionCallExpr $
          PositionalStructExprOrFunctionCall
            { pseofcNameAccessChain = LocalNameAccessChain functionName,
              pseofcTypeArgs = [],
              pseofcFields = recursiveFunctionCallArguments
            }

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
                        -- and the last (end) expression is the recursive call (if break not hit)
                        -- Note that this does not interphere with any return value, since in any case the while returns unit
                        ifThenElseIfBranch =
                          SequenceExpr $
                            Sequence
                              { sequenceUses = [],
                                sequenceItems = [SequenceItemExpr mappedBodyExpr],
                                -- If the while body contains breaks, wrap the recursion in a if(!break_hit) expression
                                -- Note that this is the only difference between the translation of continue(s) and break(s),
                                -- since this condition only checks for breaks
                                sequenceEndExpr =
                                  if containsBreak
                                    then
                                      Just $
                                        IfThenElseTerm $
                                          IfThenElse
                                            { ifThenElseCondition = UnaryOpExpr $ Negation $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "break_hit",
                                              ifThenElseIfBranch = recursiveFunctionCall,
                                              ifThenElseElseBranch = Nothing
                                            }
                                    -- Otherwise just pass the recursive call
                                    else Just recursiveFunctionCall
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
-- Helper function for both `translateWhilesToFunctions` and `translateWhilesToFunctionsInRoot`.
--
-- It is the function invoked during the post order traversal.
-- The intended execution is the following:
--
--  - Each break is substituted with an assignment to the flag variable `break_hit`
--    and the break expression is marked as having a break in it
--
--  - The same happens for a continue, that is replaced by a `continue_hit` flag
--    a single state for expressions containing either a break or a continue is used
--
--  - As the traversal moves to the root, each parent expression that has at least an expression with a break
--    is considered as having a break itself
--  - If a sequence is found an any of its sequence items (end expression excluded) have a break (or a continue):
--  - - Wrap every subsequent sequence item inside an `if (!break_hit && !continue_hit)`
--  - - Eventually, wrap the ending expression
--  - When a while is found, translate it into a function call and declaration where:
--  - - All free variables in both the body and condition of the while are function parameters as references
--  - - If the while body is marked as having a break, declare the flag variables `break_hit` and `continue_hit` and wrap the recursive call in an if-then
traversalHelper :: (Expr -> [Scope] -> ([Function], Set.Set Expr) -> (Expr, ([Function], Set.Set Expr)))
--
--  When a while loop is found, translate it
traversalHelper (WhileTerm whileLoop@While {whileExpr}) scopes' (functionDecls, exprsWithBreak) =
  let (functionCall, functionDecl) = mapWhileToFunction whileLoop scopes' (Set.member whileExpr exprsWithBreak) (Identifier $ "mapped_while_" ++ show (length functionDecls))
   in (PositionalStructExprOrFunctionCallExpr functionCall, (functionDecl : functionDecls, exprsWithBreak))
--
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
--
--  Similar when a continue is found
traversalHelper Continue _ (functionDecls, exprsWithBreak) =
  -- Replace it with an assignment `break_hit = true`
  let expr' =
        AssignmentExpr $
          Assignment
            { assignmentLeft = NameAccessChainExpr $ LocalNameAccessChain $ Identifier "continue_hit",
              assignmentRight = ValueLiteral $ Boolean True
            }
   in (expr', (functionDecls, Set.insert expr' exprsWithBreak))
--
-- When a SequenceExpr is found
traversalHelper (SequenceExpr Sequence {sequenceUses, sequenceItems, sequenceEndExpr}) _ (functionDecls, exprsWithBreak) =
  let -- Helper variable that represent the binary expression `!break_hit && !continue_hit` used in the translation logic
      breakAndContinueGuardCondition =
        BinaryOpExprExpr $
          And
            (UnaryOpExpr $ Negation $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "break_hit")
            (UnaryOpExpr $ Negation $ NameAccessChainExpr $ LocalNameAccessChain $ Identifier "continue_hit")

      -- Check if any of the inner sequence items contain a break (or a continue). If so, all the subsequent items need to be inserted inside an if
      (mappedSequenceItems, anySeqItemHasBreak) = foldr mapSeqItem ([], False) sequenceItems
        where
          mapSeqItem seqItemExpr (subseqItems, anySeqItemHasBreak'') =
            -- Internal function that, given the expression inside a sequence item,
            -- returns the correct sequence items and state depending if the current expression has a break inside or not
            let mapItem currExpr =
                  if Set.member currExpr exprsWithBreak && not (null subseqItems)
                    -- The if-then should be created only if there are sequence items to wrap
                    then
                      let ifThenElse =
                            SequenceItemExpr $
                              IfThenElseTerm $
                                IfThenElse
                                  { ifThenElseCondition = breakAndContinueGuardCondition,
                                    ifThenElseIfBranch =
                                      SequenceExpr $
                                        Sequence
                                          { sequenceUses = [],
                                            sequenceItems = subseqItems,
                                            sequenceEndExpr = Nothing
                                          },
                                    ifThenElseElseBranch = Nothing
                                  }
                       in ([seqItemExpr, ifThenElse], True)
                    -- Otherwise, append this item and still check if it has a break
                    else (seqItemExpr : subseqItems, anySeqItemHasBreak'' || Set.member currExpr exprsWithBreak)
             in -- The action to take is the same independently if the sequence item is an expression or a binding
                case seqItemExpr of
                  SequenceItemExpr itemExpr -> mapItem itemExpr
                  SequenceItemBindExpr Bindings {bindingsBindExpr = Just bindingsBindExpr} -> mapItem bindingsBindExpr
                  _ -> (seqItemExpr : subseqItems, anySeqItemHasBreak'')

      -- Then, handle the ending expression
      -- This expression might be wrapped if any of the previous sequence items have a break (or a continue)
      -- And in any case, it might contain a break itself, that must be forwarded to the entire sequence
      (sequenceEndExpr', anySeqItemHasBreak') = case sequenceEndExpr of
        Nothing -> (sequenceEndExpr, anySeqItemHasBreak)
        Just sequenceEndExpr'' ->
          if anySeqItemHasBreak
            then
              let newEndExpr =
                    IfThenElseTerm $
                      IfThenElse
                        { ifThenElseCondition = breakAndContinueGuardCondition,
                          ifThenElseIfBranch = sequenceEndExpr'',
                          ifThenElseElseBranch = Nothing
                        }
               in (Just newEndExpr, True)
            else (sequenceEndExpr, Set.member sequenceEndExpr'' exprsWithBreak)

      -- Create the new sequence expression
      newSequence =
        SequenceExpr $
          Sequence
            { sequenceUses = sequenceUses,
              sequenceItems = mappedSequenceItems,
              sequenceEndExpr = sequenceEndExpr'
            }
      -- And if necessary, add it to the set of expressions that have a break
      exprsWithBreak' = if anySeqItemHasBreak' then Set.insert newSequence exprsWithBreak else exprsWithBreak
   in -- Return a new sequence
      -- No additional functions should be declared, the set of expressions containing a break might have been updated
      (newSequence, (functionDecls, exprsWithBreak'))
--
--  Otherwise, simply check if the inner expressions have a break in them
--  If so, this expression should also be marked as having a break
traversalHelper expr _ (functionDecls, exprsWithBreak) =
  let subExpr = children expr
      hasBreak = any (`Set.member` exprsWithBreak) subExpr

      exprsWithBreak' = if hasBreak then Set.insert expr exprsWithBreak else exprsWithBreak
   in (expr, (functionDecls, exprsWithBreak'))

-- |
-- Given an expression, recursively converts each while loop into a function declaration,
-- and substitutes the loop expression with that function call
-- Also see documentation of `mapWhileToFunction`
translateWhilesToFunctions :: Expr -> [Scope] -> (Expr, [Function])
translateWhilesToFunctions expr scopes =
  let (expr', (functionDecls, _)) = traverseExprPostOrder traversalHelper traversalBindingsIdentity expr scopes ([], Set.empty)
   in (expr', functionDecls)

-- |
-- Given a module or a script, translates all while loops into function calls.
--
-- The corresponding function declarations are added inside the module or script
-- Also see documentation of `mapWhileToFunction`
translateWhilesToFunctionsInRoot :: Root -> Root
translateWhilesToFunctionsInRoot root = case traverseRootPostOrder traversalHelper traversalBindingsIdentity root ([], Set.empty) of
  (RModule translatedModule@Module {moduleTopLevels}, (functionDecls, _)) ->
    let newTopLevels = map TopLevelFunction functionDecls
     in RModule $ translatedModule {moduleTopLevels = newTopLevels ++ moduleTopLevels}
  (RScript translatedScript@Script {scriptTopLevels}, (functionDecls, _)) ->
    let newTopLevels = map TopLevelFunction functionDecls
     in RScript $ translatedScript {scriptTopLevels = newTopLevels ++ scriptTopLevels}
