{
module Move.Lexer (scan) where

import Move.Token
}

%wrapper "posn"

$digit = [0-9]
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]

tokens :-
  -- Ignored tokens
  $white+                       ; -- skip white space
  "//".*                        ; -- skip comments
  -- Separators
  \(                            { \_ _ -> TokenSeparatorLParen              }
  \)                            { \_ _ -> TokenSeparatorRParen              }
  \{                            { \_ _ -> TokenSeparatorLBrace              }
  \}                            { \_ _ -> TokenSeparatorRBrace              }
  \,                            { \_ _ -> TokenSeparatorComma               }
  \:                            { \_ _ -> TokenSeparatorColon               }
  \;                            { \_ _ -> TokenSeparatorSemiColon           }
  \:\:                          { \_ _ -> TokenSeparatorDColon              }
  -- Literals
  $digit+                       { \_ s -> TokenLiteralIntDec (read s)       }
  0x$hex+                       { \_ s -> TokenLiteralIntHex s              }
  \"($digit|$alpha)*\"          { \_ s -> TokenLiteralString s              }
  true                          { \_ _ -> TokenLiteralBool True             } 
  false                         { \_ _ -> TokenLiteralBool False            }
  -- Keywords: Module, Script
  const                         { \_ _ -> TokenKeywordConst                 }
  friend                        { \_ _ -> TokenKeywordFriend                }
  fun                           { \_ _ -> TokenKeywordFun                   }
  module                        { \_ _ -> TokenKeywordModule                }
  script                        { \_ _ -> TokenKeywordScript                }
  use                           { \_ _ -> TokenKeywordUse                   }
  -- Keywords: Structs
  struct                        { \_ _ -> TokenKeywordStruct                }
  has                           { \_ _ -> TokenKeywordHas                   }
  key                           { \_ _ -> TokenKeywordKey                   }
  store                         { \_ _ -> TokenKeywordStore                 }
  drop                          { \_ _ -> TokenKeywordDrop                  }
  copy                          { \_ _ -> TokenKeywordCopy                  }
  -- Keywords: Control flow
  if                            { \_ _ -> TokenKeywordIf                    }
  else                          { \_ _ -> TokenKeywordElse                  }
  while                         { \_ _ -> TokenKeywordWhile                 }
  loop                          { \_ _ -> TokenKeywordLoop                  }
  break                         { \_ _ -> TokenKeywordBreak                 }
  continue                      { \_ _ -> TokenKeywordContinue              }
  -- Keywords: Let binding
  let                           { \_ _ -> TokenKeywordLet                   }
  in                            { \_ _ -> TokenKeywordIn                    }
  -- Operators
  \+                            { \_ _ -> TokenOperatorPlus                 }
  \-                            { \_ _ -> TokenOperatorMinus                }
  \*                            { \_ _ -> TokenOperatorTimes                }
  \/                            { \_ _ -> TokenOperatorDiv                  }
  \%                            { \_ _ -> TokenOperatorMod                  }
  \=\=                          { \_ _ -> TokenOperatorEq                   }
  \!\=                          { \_ _ -> TokenOperatorNeq                  }
  \<                            { \_ _ -> TokenOperatorLt                   }
  \<\=                          { \_ _ -> TokenOperatorLeq                  }
  \>                            { \_ _ -> TokenOperatorGt                   }
  \>\=                          { \_ _ -> TokenOperatorGeq                  }
  \&\&                          { \_ _ -> TokenOperatorAnd                  }
  \|\|                          { \_ _ -> TokenOperatorOr                   }
  \!                            { \_ _ -> TokenOperatorNot                  }
  \=                            { \_ _ -> TokenOperatorAssign               }
  \&                            { \_ _ -> TokenOperatorRef                  }
  \.                            { \_ _ -> TokenOperatorDot                  }
  \@                            { \_ _ -> TokenOperatorAt                   }
  -- Identifiers
  $alpha($alpha | $digit)*      { \_ s -> TokenIdentifier s                 }

{
scan :: String -> [Token]
scan = alexScanTokens
}