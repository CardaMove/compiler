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
  : Use                 { TopLevelUse $1 }


-- Use TODO: importing members and aliasing them
Use :: { Use }
  : use Address '::' Identifier ';' {
    Use {
      useAddress = $2,
      useName = $4,
      useAlias = $4 -- `use std::vector;` is equivalent to `use std::vector as vector;`
    }
  }
  | use Address '::' Identifier as Identifier ';' {
    Use {
      useAddress = $2,
      useName = $4,
      useAlias = $6
    }
  }

{-
-- Struct

Struct :: { Struct }
  : struct Identifier HasAbilities '{' Fields '}'  {
      Struct {
        structIdentifier = $2,
        structAbilities = $3,
        structFields = $5
      }
    }

HasAbilities :: { [Ability] }
  : {- empty -}   { [] }
  | has Abilities { $2 }

Abilities ::  { [Ability] }
  : Ability               { [$1] }
  | Abilities ',' Ability { $3 : $1 }

Ability ::  { Ability }
  : copy  { Copy }
  | drop  { Drop }
  | key   { Key }
  | store { Store }

Fields :: { [Field] }
  : {- empty -}       { [] }
  | Field             { [$1] }
  | Fields ',' Field  { $3 : $1 }

Field ::  { Field }
  : Identifier ':' identifier {
      Field {
        fieldIdentifier = $1,
        fieldType = TypeName $3
      }
    }
 -}
{-
Uses :: { [Use] }
  : Use { [$1] }
  | Uses Use { $2 : $1 }

Use :: { Use }
  : use identifier '::' identifier { Use (AddressNamed $2) (Identifier $4) }

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