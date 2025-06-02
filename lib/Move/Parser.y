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
  ','       { TokenSeparatorComma       }
  ':'       { TokenSeparatorColon       }
  ';'       { TokenSeparatorSemiColon   }
  '::'      { TokenSeparatorDColon      }
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
  loop      { TokenKeywordLoop          }
  break     { TokenKeywordBreak         }
  continue  { TokenKeywordContinue      }
  -- Keywords: Let binding
  let       { TokenKeywordLet           }
  in        { TokenKeywordIn            }
  -- Operators
  '+'       { TokenOperatorPlus         }
  '-'       { TokenOperatorMinus        }
  -- Identifiers
  symbol    { TokenIdentifier $$        }

%right in

%%

-- Module

Module :: { Module }
  : module symbol '::' symbol '{' TopLevels '}' { 
      Module {
        moduleAddress = $2,
        moduleIdentifier = $4,
        moduleTopLevels = $6
      }
    }

TopLevels :: { [TopLevel] }
  : {- empty -}         { [] }
  | TopLevel            { [$1] }
  | TopLevels TopLevel  { $2 : $1 }

TopLevel :: { TopLevel }
  : Struct  { TopLevelStruct $1 }

-- Struct

Struct :: { Struct }
  : struct symbol HasAbilities '{' Fields '}'  {
      Struct {
        structIdentifier = $2,
        structFields = $5,
        structAbilities = $3
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
  : symbol ':' symbol {
      Field {
        fieldIdentifier = $1,
        fieldType = $3
      }
    }

{-
Uses :: { [Use] }
  : Use { [$1] }
  | Uses Use { $2 : $1 }

Use :: { Use }
  : use symbol '::' symbol { Use (AddressNamed $2) (Identifier $4) }

Constants :: { [Constant] }
  : Constant { [$1] }
  | Constants Constant { $2 : $1 }

Constant :: { Constant }
  : const symbol ':' symbol '=' Expr { Constant (Identifier $2) (Type $4) $6 }

Expr :: { Expr }
  : symbol { Var (Identifier $1) }
  | let symbol '=' Expr in Expr { Let (Identifier $2) $4 $6 }

Stmts :: { [Stmt] }
  : Expr { [Stmt $1] }
  | Stmts ';' Expr { (Stmt $3) : $1 }

Function :: { Function }
  : fun symbol '(' Args ')' ':' symbol '{' Stmts '}' { Function (Identifier $2) $4 (Type $7) $9 }

Args :: { [(Identifier, Type)] }
  : {- empty -} { [] }
  | Arg { [$1] }
  | Args ',' Arg { $3 : $1 }

Arg :: { (Identifier, Type) }
  : symbol ':' symbol { (Identifier $1, Type $3) }
-}

{
onError :: [Token] -> e
onError tokens = error $ "Parse error on tokens: " ++ show tokens
}