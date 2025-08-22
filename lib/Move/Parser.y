{
module Move.Parser (parse) where

import Move.AST
import Move.Lexer
import Move.Token
}

%name parse
%tokentype { Token }
%error { onError }

%token
  -- Separators
  '('       { TokenSeparatorLParen      }
  ')'       { TokenSeparatorRParen      }
  '{'       { TokenSeparatorLBrace      }
  '}'       { TokenSeparatorRBrace      }
  '['       { TokenSeparatorLSquareBracket }
  ']'       { TokenSeparatorRSquareBracket }
  ','       { TokenSeparatorComma       }
  ':'       { TokenSeparatorColon       }
  ';'       { TokenSeparatorSemiColon   }
  '::'      { TokenSeparatorDoubleColon      }
  -- Literals
  int       { TokenLiteralIntDec $$     }
  hex       { TokenLiteralIntHex $$     }
  string    { TokenLiteralString $$     }
  true      { TokenLiteralBool True     }
  false     { TokenLiteralBool False    }
  -- Keywords: Module, Script
  const     { TokenKeywordConst         }
  friend    { TokenKeywordFriend        }
  fun       { TokenKeywordFun           }
  module    { TokenKeywordModule        }
  script    { TokenKeywordScript        }
  use       { TokenKeywordUse           }
  -- Keywords: Functions
  public    { TokenKeywordPublic        }
  package   { TokenKeywordPackage       }
  entry     { TokenKeywordEntry         }
  acquires  { TokenKeywordAcquires      }
  native    { TokenKeywordNative        }
  -- Keywords: Structs
  struct    { TokenKeywordStruct        }
  has       { TokenKeywordHas           }
  key       { TokenKeywordKey           }
  store     { TokenKeywordStore         }
  drop      { TokenKeywordDrop          }
  copy      { TokenKeywordCopy          }
  -- Keywords: Control flow
  if        { TokenKeywordIf            }
  else      { TokenKeywordElse          }
  while     { TokenKeywordWhile         }
  for       { TokenKeywordFor           }
  loop      { TokenKeywordLoop          }
  break     { TokenKeywordBreak         }
  continue  { TokenKeywordContinue      }
  return    { TokenKeywordReturn        }
  abort     { TokenKeywordAbort         }
  -- Other keywords
  let       { TokenKeywordLet           }
  phantom   { TokenKeywordPhantom       }
  as        { TokenKeywordAs            }
  move      { TokenKeywordMove          }
  -- Operators
  '+'       { TokenOperatorPlus         }
  '-'       { TokenOperatorMinus        }
  '*'       { TokenOperatorTimes        }
  '/'       { TokenOperatorDiv          }
  '%'       { TokenOperatorMod          }
  '=='      { TokenOperatorEq           }
  '!='      { TokenOperatorNeq          }
  '<'       { TokenOperatorLt           }
  '<='      { TokenOperatorLeq          }
  '>'       { TokenOperatorGt           }
  '>='      { TokenOperatorGeq          }
  '&&'      { TokenOperatorAnd          }
  '||'      { TokenOperatorOr           }
  '!'       { TokenOperatorNot          }
  '='       { TokenOperatorAssign       }
  '&'       { TokenOperatorAmp          }
  '|'       { TokenOperatorBitwiseOr    }
  '^'       { TokenOperatorBitwiseXor   }
  '&mut'    { TokenOperatorAmpMut       }
  '.'       { TokenOperatorDot          }
  '..'      { TokenOperatorDoubleDot    }
  '@'       { TokenOperatorAt           }
  '<<'      { TokenOperatorShiftLeft    }
  '>>'      { TokenOperatorShiftRight   }
  -- Identifiers
  identifier{ TokenIdentifier $$        }


-- Fixes precedence at State 102 between Expr -> BinaryOpExpr . and the others
%nonassoc LOWER

-- In conjunction with LOWER, fixes multiple precedences on State 243
-- Confronting the before-after produced grammar, only that state is affected
%right '('
%right '{'
%nonassoc int
%nonassoc hex
%nonassoc true
%nonassoc false
%right copy
%right if
%left while
%right loop
%left break
%left continue
%right abort
%right move
%right '!'
%right '&mut'
%right '@'
%right identifier

-- In conjunction with %prec LOWER at rule 95, fixes precedence at State 103 when encountering a '='
--    Expr -> UnaryExpr . '=' Expr                        (rule 76)
--   	BinaryOpExpr -> UnaryExpr .                         (rule 95)
%left '='

-- Fixes Return precedence when encountering * or & after return keyword:
--    Return -> return .                                  (rule 121)
--    Return -> return . Expr                             (rule 122)
--    Return -> return . '{' Expr '}'                     (rule 123)
-- Must occur prior of '*' and '&' (for some reasons, not working with custom precedence terms)
%right return

%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%left '|' 
%left '^' 
%left '&' 
%left '<<'
%left '>>'
%left '+' '-' 
%left '*' '/' '%' 

%right OPT_TYPE_ARGS

-- Fixes DotOrIndexChain precedence:
--    UnaryExpr -> DotOrIndexChain .                      (rule 101)
--    DotOrIndexChain -> DotOrIndexChain . '.' Identifier    (rule 102)
%left DOT_OR_INDEX_CHAIN
%left '.'

-- In conjunction with %prec, fixes the dangling else problem (with and without braces):
--    Term -> if '(' Expr ')' Expr .                      (rule 110)
--    Term -> if '(' Expr ')' Expr . else Expr            (rule 111)
-- Also see Bison manual at "5.3.6 Using Precedence For Non Operators"
%right IF_NO_ELSE
%right else

%%

-- Module
Module :: { Module }
  : module Address '::' Identifier '{' TopLevels '}' {
      Module {
        moduleAddress = $2,
        moduleIdentifier = $4,
        moduleTopLevels = $6
      }
    }

-- Wrapper for an identifier
Identifier :: { Identifier }
  : identifier { Identifier $1 }

-- Module top levels
TopLevels :: { [TopLevel] }
  : TopLevels_          { reverse $1 } -- Reverse the left-recursive rule

TopLevels_ :: { [TopLevel] }
  : {- empty -}         { [] }
  | TopLevels_ TopLevel  { $2 : $1 } -- Note: Left-recursive rule

TopLevel :: { TopLevel }
  : Use                      { TopLevelUse $1 }
  | Friend                   { TopLevelFriend $1 }
  | NamedStruct              { TopLevelNamedStruct $1 }
  | PositionalStruct         { TopLevelPositionalStruct $1 }
  | Function                 { TopLevelFunction $1 }
  | Constant                 { TopLevelConstant $1 }


-- Use
Use :: { Use }
  : use Address '::' Identifier OptionalUseAlias ';'                  { Use { useAddress = $2, useIdentifier = $4, useAlias = $5, useMembers = [] } }
  -- Single aliased member
  | use Address '::' Identifier '::' UseMember ';'                    { Use { useAddress = $2, useIdentifier = $4, useAlias = Nothing, useMembers = [$6] } }
  | use Address '::' Identifier '::' '{' CommaUseMember '}' ';'       { Use { useAddress = $2, useIdentifier = $4, useAlias = Nothing, useMembers = reverse $7 } } -- Reverse the left-recursive rule


OptionalUseAlias :: { Maybe Identifier }
  : {- empty -}                   { Nothing }
  | as Identifier                 { Just $2 }


UseMember :: { UseMember }
  : Identifier OptionalUseAlias            { UseMember { useMemberIdentifier = $1, useMemberUseAlias = $2 } }


CommaUseMember :: { [UseMember] }
  : UseMember                               { [$1] }
  | CommaUseMember ',' UseMember            { $3 : $1 }


-- A sequence of multiple use declarations. Present in function bodies
OptionalUses :: { [Use] }
  : OptionalUses_                       { reverse $1 }  -- Reverse the left-recursive rule


OptionalUses_ :: { [Use] }
  : {- empty -}                         { [] }
  | OptionalUses_ Use                   { $2 : $1 }


-- Friend
Friend :: { Friend }
  : friend NameAccessChain ';'            { Friend $2 }


-- Named struct
NamedStruct :: { NamedStruct }
  : struct Identifier OptionalTypeParameters HasAbilities '{' NamedFields '}'  { -- named structs do not end with ;
      NamedStruct {
        namedStructIdentifier = $2,
        namedStructTypeParameters = $3,
        namedStructAbilities = $4,
        namedStructFields = $6
      }
    }


-- Positional struct
PositionalStruct :: { PositionalStruct }
  -- struct Foo has copy, drop;
  : struct Identifier OptionalTypeParameters HasAbilities ';' { -- positional structs must end with ;
    PositionalStruct {
        positionalStructIdentifier = $2,
        positionalStructTypeParameters = $3,
        positionalStructAbilities = $4,
        positionalStructFields = []
      }
  }
  -- struct Foo(A, B) has copy, drop;
  | struct Identifier OptionalTypeParameters '(' PositionalFields ')' HasAbilities ';' {
      PositionalStruct {
        positionalStructIdentifier = $2,
        positionalStructTypeParameters = $3,
        positionalStructAbilities = $7,
        positionalStructFields = $5
      }
    }


-- Abilities of a struct
HasAbilities :: { [Ability] }
  : {- empty -}   { [] }
  | has Abilities { $2 }

Abilities :: { [Ability] }
  : Abilities_          { reverse $1 } -- Reverse the left-recursive rule

Abilities_ ::  { [Ability] }
  : Ability               { [$1] }
  | Abilities_ ',' Ability { $3 : $1 }

Ability ::  { Ability }
  : copy  { Copy }
  | drop  { Drop }
  | key   { Key }
  | store { Store }


-- Named fields of a struct
NamedFields :: { [NamedField] }
  : NamedFields_          { reverse $1 } -- Reverse the left-recursive rule

NamedFields_ :: { [NamedField] }
  : {- empty -}       { [] }
  | NamedField             { [$1] }
  | NamedFields_ ',' NamedField  { $3 : $1 }

NamedField ::  { NamedField }
  : Identifier ':' Type {
      NamedField {
        fieldIdentifier = $1,
        fieldType = $3  
      }
    }


-- | Represents any type: type constructow with arguments, reference types, tuple types
Type :: { Type }
  : NameAccessChain OptionalTypeArgs        { TypeConstructor $1 $2 }
  | '&' Type                                { TypeImmutableRef $2 }
  | '&mut' Type                             { TypeMutableRef $2 }
  | '(' CommaType_ ')'                      { TypeTuple $ reverse $2 }  -- Reverse the left-recursive rule


OptionalTypeArgs :: { [Type] }
  : {- empty -}                        { [] }
  | '<' CommaType_ '>'                 { reverse $2 }  -- Reverse the left-recursive rule


CommaType_ :: { [Type] }
  : Type                               { [$1] }
  | CommaType_ ',' Type                { $3 : $1 }


-- Positional fields of a struct
PositionalFields :: { [PositionalField] }
  : PositionalFields_                   { reverse $1 } -- Reverse the left-recursive rule

PositionalFields_ :: { [PositionalField] }
  : {- empty -}                         { [] }
  | Type                                { [PositionalField $1] } 
  | PositionalFields_ ',' Type          { (PositionalField $3) : $1 }


-- Function declaration
Function :: { Function }
  -- native function (no body, must end with ;)
  : native VisibilityModifier EntryModifier fun Identifier OptionalTypeParameters '(' FunctionParameters ')' FunctionReturnType FunctionAcquires ';' {
    Function {
      functionHasNativeModifier = True,
      functionVisibilityModifier = $2,
      functionHasEntryModifier = $3,
      functionName = $5,
      functionTypeParameters = $6,
      functionParameters = $8,
      functionReturnType = $10,
      functionAcquires = $11,
      functionBody = Nothing
    }
  }
  -- non-native function
  | VisibilityModifier EntryModifier fun Identifier OptionalTypeParameters '(' FunctionParameters ')' FunctionReturnType FunctionAcquires Sequence {
    Function {
      functionHasNativeModifier = False,
      functionVisibilityModifier = $1,
      functionHasEntryModifier = $2,
      functionName = $4,
      functionTypeParameters = $5,
      functionParameters = $7,
      functionReturnType = $9,
      functionAcquires = $10,
      functionBody = Just $11
    }
  }


VisibilityModifier :: { Maybe VisibilityModifier }
  : {- empty -}                     { Nothing }
  | public                          { Just VisibilityModifierPublic }
  | package                         { Just VisibilityModifierPackage }
  -- Old and new notations for friend visibility
  | public '(' friend ')'           { Just VisibilityModifierFriend }
  | friend                          { Just VisibilityModifierFriend }


EntryModifier :: { Bool }
  : {- empty -}         { False }
  | entry               { True }

-- Type params (both for functions and structs)
OptionalTypeParameters :: {  [TypeParameter] }
  : {- empty -}             { [] }
  | '<' TypeParameters '>'  { $2 }

TypeParameters :: { [TypeParameter] }
  : TypeParameters_       { reverse $1 } -- Reverse left-recursive rule

TypeParameters_ :: { [TypeParameter] }
  : TypeParameter                          { [$1] }
  | TypeParameters_ ',' TypeParameter      { $3 : $1 }

TypeParameter :: { TypeParameter }
  : TypeParameterPhantom Identifier TypeParameterConstraints         {
      TypeParameter {
        typeParameterIsPhantom = $1,
        typeIdentifier = $2,
        typeConstraints = $3
      }
    } 


TypeParameterPhantom :: { Bool }
  : {- empty -}         { False }
  | phantom             { True }


TypeParameterConstraints :: { [Ability] }
  : {- empty -}   { [] }
  | ':' TypeParameterAbilities { reverse $2 }    -- Reverse the left-recursive rule

TypeParameterAbilities ::  { [Ability] }
  : Ability               { [$1] }
  | TypeParameterAbilities '+' Ability { $3 : $1 }   -- Similar to structs abilities but with + separator


-- Function parameters
FunctionParameters :: {[Parameter]}
  : {- empty -}             { [] }
  | FunctionParameters_     { reverse $1 }  -- Reverse left-recursive rule

FunctionParameters_ :: { [Parameter] }
  : FunctionParameter                             { [$1] }
  | FunctionParameters_ ',' FunctionParameter     { $3 : $1 }

FunctionParameter :: { Parameter }
  : Identifier ':' Type             {
    Parameter {
      parameterIdentifier = $1,
      parameterType = $3      
    }
  }


-- Function return type
FunctionReturnType :: { Maybe Type }
  : {- empty -}           { Nothing }
  | ':' Type              { Just $2 }    


-- Acquires can only be resource names, not including type arguments
FunctionAcquires :: { [NameAccessChain] }
  : {- empty -}                       { [] }
  | acquires FunctionAcquires_        { reverse $2 }      -- Reverse left-recursive rule


FunctionAcquires_ :: { [NameAccessChain] }
  : NameAccessChain                           { [$1] }
  | FunctionAcquires_ ',' NameAccessChain     { $3 : $1 }  


-- Constants
Constant :: { Constant }
  : const Identifier ':' Type '=' Expr ';'         {
    Constant {
      constantIdentifier = $2,
      constantType = $4,
      constantExpression = $6
    }
  }


-- Expressions
Expr :: { Expr }
  : BinaryOpExpr      %prec LOWER                        { $1 }
  | UnaryExpr '=' Expr                                   { AssignmentExpr $ Assignment { assignmentLeft = $1, assignmentRight = $3 } }


-- Binary operations (listed from lowest to highest precedence)
BinaryOpExpr :: { Expr }
  : BinaryOpExpr '||' BinaryOpExpr                       { BinaryOpExprExpr $ Or $1 $3 }
  | BinaryOpExpr '&&' BinaryOpExpr                       { BinaryOpExprExpr $ And $1 $3 }
  | BinaryOpExpr '==' BinaryOpExpr                       { BinaryOpExprExpr $ Eq $1 $3 }
  | BinaryOpExpr '!=' BinaryOpExpr                       { BinaryOpExprExpr $ Neq $1 $3 }
  | BinaryOpExpr '<' BinaryOpExpr                        { BinaryOpExprExpr $ Lt $1 $3 }
  | BinaryOpExpr '>' BinaryOpExpr                        { BinaryOpExprExpr $ Gt $1 $3 }
  | BinaryOpExpr '<=' BinaryOpExpr                       { BinaryOpExprExpr $ Leq $1 $3 }
  | BinaryOpExpr '>=' BinaryOpExpr                       { BinaryOpExprExpr $ Geq $1 $3 }
  | BinaryOpExpr '|' BinaryOpExpr                        { BinaryOpExprExpr $ BitwiseOr $1 $3 }
  | BinaryOpExpr '^' BinaryOpExpr                        { BinaryOpExprExpr $ BitwiseXor $1 $3 }
  | BinaryOpExpr '&' BinaryOpExpr                        { BinaryOpExprExpr $ BitwiseAnd $1 $3 }
  | BinaryOpExpr '<<' BinaryOpExpr                       { BinaryOpExprExpr $ ShiftLeft $1 $3 }
  | BinaryOpExpr '>>' BinaryOpExpr                       { BinaryOpExprExpr $ ShiftRight $1 $3 }
  | BinaryOpExpr '+' BinaryOpExpr                        { BinaryOpExprExpr $ Add $1 $3 }
  | BinaryOpExpr '-' BinaryOpExpr                        { BinaryOpExprExpr $ Sub $1 $3 }
  | BinaryOpExpr '*' BinaryOpExpr                        { BinaryOpExprExpr $ Mult $1 $3 }
  | BinaryOpExpr '/' BinaryOpExpr                        { BinaryOpExprExpr $ Div $1 $3 }
  | BinaryOpExpr '%' BinaryOpExpr                        { BinaryOpExprExpr $ Mod $1 $3 }
  | UnaryExpr                         %prec LOWER        { $1 }


UnaryExpr :: { Expr }
  : '!' UnaryExpr                                        { UnaryOpExpr $ Negation $2 }
  | '&mut' UnaryExpr                                     { UnaryOpExpr $ MutableReference $2 }
  | '&' UnaryExpr                                        { UnaryOpExpr $ ImmutableReference $2 }
  -- Dereference
  | '*' UnaryExpr                                        { UnaryOpExpr $ Dereference $2 }
  | move Identifier                                      { UnaryOpExpr $ MoveExpr $2 }
  | copy Identifier                                      { UnaryOpExpr $ CopyExpr $2 }
  | DotOrIndexChain    %prec DOT_OR_INDEX_CHAIN          { $1 }


DotOrIndexChain :: { Expr }
  : DotOrIndexChain '.' Identifier                       { DotOrIndexChainExpr $ DotAccess { dotAccessLeft = $1, dotAccessRight = $3 } }
  | Term                                                 { $1 }


Term :: { Expr }
  : break                                                { Break }
  | continue                                             { Continue }
  -- TODO: vector
  | NameExpr                                             { $1 }
  | Value                                                { ValueLiteral $1 }
  -- A tuple value
  | '(' CommaExpr ')'                                    { CommaExpr $2 }
  -- Explicit typing 
  | '(' Expr ':' Type ')'                                { TypedExprTerm $ TypedExpr { typedExpr = $2, typedExprType = $4 } }
  -- Casting
  | '(' Expr as Type ')'                                 { CastingTerm $ Casting { castingExpr = $2, castingType = $4 } }
  | Sequence                                             { SequenceExpr $1 }
  -- if then else 
  | if '(' Expr ')' Expr                            %prec IF_NO_ELSE                { IfThenElseTerm $ IfThenElse { ifThenElseCondition = $3, ifThenElseIfBranch = $5, ifThenElseElseBranch = Nothing } }
  | if '(' Expr ')' Expr else Expr                                                  { IfThenElseTerm $ IfThenElse { ifThenElseCondition = $3, ifThenElseIfBranch = $5, ifThenElseElseBranch = Just $7 } }
  -- while
  | while '(' Expr ')' Expr                              { WhileTerm $ While { whileCondition = $3, whileExpr = $5 }}
  -- loop
  | loop Expr                                            { Loop $2 }
  -- FIXME: where is for?
  | Return                                               { $1 }
  | abort Expr                                           { Abort $2 }


Return :: { Expr }
  : return                                               { Return Nothing }
  | return Expr                                          { Return $ Just $2 }


-- A sequence of comma-separated expressions. Used for tuples and function calls.
-- Includes unit () and single (val)
-- In fact, this should be the only Comma rule that allows for no members (empty list)
CommaExpr :: { [Expr] }
  : {- empty -}                { [] }
  | CommaExpr_                 { reverse $1 } -- Reverse the left-recursive rule

CommaExpr_ :: { [Expr] }
  : Expr                       { [$1] }
  | CommaExpr_ ',' Expr        { $3 : $1 }


-- Any literal value
Value :: { ValueLiteral }
  : '@' Address           { Address $2 }
  | false                 { Boolean False }
  | true                  { Boolean True }
  | Numerical             { Numerical $1 }
  -- TODO: number typed?
  -- TODO: Byte strings and hex strings



-- Address values
Address :: { Address }
  : Identifier            { NamedAddress $1 }
  | Numerical             { NumericalAddress $1 }


-- Numerical values
Numerical :: { Numerical }
  : int                   { LiteralIntDec $1 }
  | hex                   { LiteralIntHex $1 }


-- Literal struct (named or positional), function call, function call with !, variable
NameExpr :: { Expr }
  -- Literal named struct
  : NameAccessChain OptionalTypeArgs '{' NamedStructExprFields '}'        { NamedStructExprExpr $ NamedStructExpr {
      nseNameAccessChain = $1,
      nseTypeArgs = $2,
      nseFields = $4
    } }
  -- It's not possible to distinguish a positional struct wrt a function call just by parsing
  -- Example:
  --    `let a = GuessWhoAmI(42, "unknown");`
  | NameAccessChain OptionalTypeArgs '(' CommaExpr ')'                    { PositionalStructExprOrFunctionCallExpr $ PositionalStructExprOrFunctionCall {
    pseofcNameAccessChain = $1,
    pseofcTypeArgs = $2,
    pseofcFields = $4
  } }
  -- A function call with a bang! before the left parenthesis. Used by assert!()
  | NameAccessChain '!' '(' CommaExpr ')'                                 { FunctionBangCallExpr $ FunctionBangCall {
    fbcNameAccessChain = $1,
    fbcFields = $4
  } }
  -- This is a variable!
  -- FIXME: Ambiguity with < considered type arguments or less than operation
  | NameAccessChain                            %prec LOWER                { NameAccessChainExpr $1 }                      


-- A sequence of fields of a literal struct
NamedStructExprFields :: { [NamedStructExprField] }
  : {- empty -}                         { [] }
  | NamedStructExprFields_              { reverse $1 }


NamedStructExprFields_ :: { [NamedStructExprField] }
  : NamedStructExprField                                      { [$1] }
  | NamedStructExprFields_ ',' NamedStructExprField           { $3 : $1 }


-- A field of a named struct can either be the identifier alone, or with an expression
NamedStructExprField :: { NamedStructExprField }
  : Identifier                      { NamedStructExprField { nsefIdentifier = $1, nsefExpr = Nothing } }     
  | Identifier ':' Expr             { NamedStructExprField { nsefIdentifier = $1, nsefExpr = Just $3 } }


-- A name access chain is an access to a variable, struct or function that might be declared on another module
--    Note: as on move-language/move/language/move-compiler/src/parser/ast.rs:419
--    It's correct to consider a single (Identifier), a tuple with (Address, Identifier) and a triple (Address, Identifier, Identifier)
NameAccessChain :: { NameAccessChain }
  -- This is a normal identifier
  : Identifier                                            { LocalNameAccessChain $1 }
  -- This is an access to an aliased module
  | Address '::' Identifier                               { AliasedNameAccessChain $1 $3 }
  | Address '::' Identifier '::' Identifier               { UnaliasedNameAccessChain $1 $3 $5 }


-- A sequence of use declarations, expressions, bindings, and optionally a final expression without ;
-- The pattern matching is used to determine if the last SequenceItem is followed or not by a ';'
-- If not, and that item matches an Expr, it will be considered the ending Expr of the sequence
--
-- Pattern 1: The last item is followed by a ';'
--    Example:
--       SequenceItem1; SequenceItem2; }
-- Pattern 2: The last item is not followed by a ';' but a '}' instead
--    Example:
--       SequenceItem1; SequenceItem2 }
--    Then, it must be an Expr
-- Pattern 3: If not an Expr, fail
Sequence :: { Sequence }
  : '{' OptionalUses SequenceItems_    %prec LOWER              {
    case $3 of
      (seqItems, False) -> Sequence {                 
        sequenceUses = $2,                            
        sequenceItems = reverse seqItems,             
        sequenceEndExpr = Nothing
      }
      (SequenceItemExpr(head) : tail, True) -> Sequence {     
        sequenceUses = $2,                                    
        sequenceItems = reverse tail,                         
        sequenceEndExpr = Just head                           
      }
      (head : _, True) -> error $ "Sequence ends with non-Expr return" ++ show head       
  }


SequenceItems_ :: { ([SequenceItem], Bool) }
  : SequenceItems__                                 { (reverse $ fst $1, snd $1) }


-- To resolve ambiguities when parsing Expr ';' with optional ';', it has been used a right recursion,
-- With a second tuple element indicating if the SequenceItems__ end with a final SequenceItem not followed by a ';'
-- If this is the case, that element will be considered the ending Expr in the Sequence pattern matching
-- FIXME: right recursion
SequenceItems__ :: { ([SequenceItem], Bool) }
  : '}'                                             { ([], False) }
  | SequenceItem '}'                                { ([$1], True) }
  | SequenceItem ';' SequenceItems__                { ($1 : (fst $3), snd $3) }


-- A sequence item can either be an expression or a binding
SequenceItem :: { SequenceItem }
  : Expr                                                          { SequenceItemExpr $1 }
  | let Bind OptionalBindType OptionalBindExpr                    { SequenceItemBindExpr $ Bindings {
    bindings = BindedSingle $2,
    bindingsBindType = $3,
    bindingsBindExpr = $4
  } }
  | let '(' CommaBind ')' OptionalBindType OptionalBindExpr       { SequenceItemBindExpr $ Bindings {
    bindings = BindedTuple $3,
    bindingsBindType = $5,
    bindingsBindExpr = $6
  } }


-- A binding can be done to either an identifier or to fields of a named struct
Bind :: { Bind }
  : Identifier                                                      { BindIdentifier $1 }
  | NameAccessChain OptionalTypeArgs '{' CommaBindedField '}'       { BindNamedStruct $ BindedNamedStruct {
    bnsNameAccessChain = $1,
    bnsTypeArgs = $2,
    bnsFields = $4
  } }
  | NameAccessChain OptionalTypeArgs '(' CommaBindedField ')'       { BindPositionalStruct $ BindedPositionalStruct {
    bpsNameAccessChain = $1,
    bpsTypeArgs = $2,
    bpsFields = $4
  } }


-- Binds separated by ,
CommaBind :: { [Bind] }
  : CommaBind_                         { reverse $1 }  -- Reverse the left-recursive rule


CommaBind_ :: { [Bind] }
  : Bind                               { [$1] }
  | CommaBind_ ',' Bind                { $3 : $1 }  


OptionalBindType :: { Maybe Type }
  : {- empty -}                       { Nothing }
  | ':' Type                          { Just $2 }


OptionalBindExpr :: { Maybe Expr }
  : {- empty -}                       { Nothing }
  | '=' Expr                          { Just $2 } 


-- Similar to NamedFields, but for binding
-- TODO: Test if possible to bind no fields {}
-- Additionally, a partial pattern can be used to skip fields
CommaBindedField :: { BindedFields }
  : CommaBindedField_                         { BindedFields {
    hasPartialPattern = hasPartialPattern $1,
    bindedFields = reverse $ bindedFields $1
  } } -- Reverse the left-recursive rule


-- Handle the case of an optional partial pattern
CommaBindedField_ :: { BindedFields }
  : BindedField                                    { 
    case $1 of
      Nothing -> BindedFields { hasPartialPattern = True, bindedFields = [] }
      Just(bf) -> BindedFields { hasPartialPattern = False, bindedFields = [bf] }
   }
  | CommaBindedField_ ',' BindedField           {
    case $3 of
      Nothing -> BindedFields {
        hasPartialPattern = True,
        bindedFields = bindedFields $1
      }
      Just(bf) -> BindedFields {
        hasPartialPattern = hasPartialPattern $1,
        bindedFields = bf : bindedFields $1
      }
  }


-- A field that is being binded is an identifier with optional inner bind
BindedField ::  { Maybe BindedField }
  : '..'                                              { Nothing }
  | Identifier                                        { Just $ BindedField { bindFieldIdentifier = $1, bindFieldInnerBind = Nothing } }
  | Identifier ':' Bind                               { Just $ BindedField { bindFieldIdentifier = $1, bindFieldInnerBind = Just $3 } }


{
onError :: [Token] -> e
onError tokens = error $ "Parse error on tokens: " ++ show tokens
}