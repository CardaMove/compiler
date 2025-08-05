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

-- Address
Address :: { Address }
  : Identifier          { NamedAddress $1 }
  | int                 { NumericalAddress (LiteralIntDec $1) }
  | hex                 { NumericalAddress (LiteralIntHex $1) }

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


-- Use TODO: importing members and aliasing them
Use :: { Use }
  : use Address '::' Identifier ';' {
    Use {
      useAddress = $2,
      useName = $4,
      useAlias = Nothing
    }
  }
  | use Address '::' Identifier as Identifier ';' {
    Use {
      useAddress = $2,
      useName = $4,
      useAlias = Just $6
    }
  }


-- Friend
Friend :: { Friend }
  : friend Address '::' Identifier ';' {
    Friend {
      friendAddress = Just $2,
      friendName = $4
    }
  }
  | friend Identifier ';' {
    Friend {
      friendAddress = Nothing,
      friendName = $2
    }
  }


-- Named struct
NamedStruct :: { NamedStruct }
  : struct Identifier AngleBracketTypeParameters HasAbilities '{' NamedFields '}'  { -- named structs do not end with ;
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
  : struct Identifier AngleBracketTypeParameters HasAbilities ';' { -- positional structs must end with ;
    PositionalStruct {
        positionalStructIdentifier = $2,
        positionalStructTypeParameters = $3,
        positionalStructAbilities = $4,
        positionalStructFields = []
      }
  }
  -- struct Foo(A, B) has copy, drop;
  | struct Identifier AngleBracketTypeParameters '(' PositionalFields ')' HasAbilities ';' {
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


-- Type with optional type parameters
Type :: { Type }
  : Identifier                         { Type $1 [] }
  | Identifier '<' TypeArgs '>'        { Type $1 $ reverse $3 }

TypeArgs :: { [Type] }
  : Type                      { [$1] }
  | TypeArgs ',' Type         { $3 : $1 }

-- Positional fields of a struct
PositionalFields :: { [PositionalField] }
  : PositionalFields_          { reverse $1 } -- Reverse the left-recursive rule

PositionalFields_ :: { [PositionalField] }
  : {- empty -}                         { [] }
  | Type                                { [PositionalField $1] } 
  | PositionalFields_ ',' Type          { (PositionalField $3) : $1 }


-- Function declaration
Function :: { Function }
  -- native function (no body, must end with ;)
  : native VisibilityModifier EntryModifier fun Identifier AngleBracketTypeParameters '(' FunctionParameters ')' FunctionReturnType FunctionAcquires ';' {
    Function {
      functionHasNativeModifier = True,
      functionVisibilityModifier = $2,
      functionHasEntryModifier = $3,
      functionName = $5,
      functionTypeParameters = $6,
      functionParameters = $8,
      functionReturnType = $10,
      functionAcquires = $11,
      functionBody = ()
    }
  }
  -- non-native function
  | VisibilityModifier EntryModifier fun Identifier AngleBracketTypeParameters '(' FunctionParameters ')' FunctionReturnType FunctionAcquires '{' FunctionBody '}' {
    Function {
      functionHasNativeModifier = False,
      functionVisibilityModifier = $1,
      functionHasEntryModifier = $2,
      functionName = $4,
      functionTypeParameters = $5,
      functionParameters = $7,
      functionReturnType = $9,
      functionAcquires = $10,
      functionBody = $12
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
AngleBracketTypeParameters :: {  [TypeParameter] }
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


-- Acquires
FunctionAcquires :: { [Type] }
  : {- empty -}                       { [] }
  | acquires FunctionAcquires_        { reverse $2 }      -- Reverse left-recursive rule

FunctionAcquires_ :: { [Type] }
  : Type                           { [$1] }
  | FunctionAcquires_ ',' Type     { $3 : $1 }  


-- Function body
FunctionBody :: { () }
  : {- empty -}       { () }

{-
Constants :: { [Constant] }
  : Constant { [$1] }
  | Constants Constant { $2 : $1 }

Constant :: { Constant }
  : const identifier ':' identifier '=' Expr { Constant (Identifier $2) (Type $4) $6 }

Expr :: { Expr }
  : identifier { Var (Identifier $1) }
  | let identifier '=' Expr in Expr { Let (Identifier $2) $4 $6 }

Stmts :: { [Stmt] }
  : Expr { [Stmt $1] }
  | Stmts ';' Expr { (Stmt $3) : $1 }

Function :: { Function }
  : fun identifier '(' Args ')' ':' identifier '{' Stmts '}' { Function (Identifier $2) $4 (Type $7) $9 }

Args :: { [(Identifier, Type)] }
  : {- empty -} { [] }
  | Arg { [$1] }
  | Args ',' Arg { $3 : $1 }

Arg :: { (Identifier, Type) }
  : identifier ':' identifier { (Identifier $1, Type $3) }
-}

{
onError :: [Token] -> e
onError tokens = error $ "Parse error on tokens: " ++ show tokens
}