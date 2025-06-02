{
module Move.Lexer (scan) where

import Move.Token
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]

tokens :-
  -- Ignored tokens
  $white+                       ; -- skip white space
  "//".*                        ; -- skip comments
  -- Separators
  \(                            { \_ -> TokenSeparatorLParen              }
  \)                            { \_ -> TokenSeparatorRParen              }
  \{                            { \_ -> TokenSeparatorLBrace              }
  \}                            { \_ -> TokenSeparatorRBrace              }
  \,                            { \_ -> TokenSeparatorComma               }
  \:                            { \_ -> TokenSeparatorColon               }
  \;                            { \_ -> TokenSeparatorSemiColon           }
  \:\:                          { \_ -> TokenSeparatorDColon              }
  -- Literals
  $digit+                       { \s -> TokenLiteralIntDec (read s)       }
  0x$hex+                       { \s -> TokenLiteralIntHex s              }
  \"($digit|$alpha)*\"          { \s -> TokenLiteralString s              }
  true                          { \_ -> TokenLiteralBool True             } 
  false                         { \_ -> TokenLiteralBool False            }
  -- Keywords: Module, Script
  const                         { \_ -> TokenKeywordConst                 }
  friend                        { \_ -> TokenKeywordFriend                }
  fun                           { \_ -> TokenKeywordFun                   }
  module                        { \_ -> TokenKeywordModule                }
  script                        { \_ -> TokenKeywordScript                }
  use                           { \_ -> TokenKeywordUse                   }
  -- Keywords: Structs
  struct                        { \_ -> TokenKeywordStruct                }
  has                           { \_ -> TokenKeywordHas                   }
  key                           { \_ -> TokenKeywordKey                   }
  store                         { \_ -> TokenKeywordStore                 }
  drop                          { \_ -> TokenKeywordDrop                  }
  copy                          { \_ -> TokenKeywordCopy                  }
  -- Keywords: Control flow
  if                            { \_ -> TokenKeywordIf                    }
  else                          { \_ -> TokenKeywordElse                  }
  while                         { \_ -> TokenKeywordWhile                 }
  loop                          { \_ -> TokenKeywordLoop                  }
  break                         { \_ -> TokenKeywordBreak                 }
  continue                      { \_ -> TokenKeywordContinue              }
  -- Keywords: Let binding
  let                           { \_ -> TokenKeywordLet                   }
  in                            { \_ -> TokenKeywordIn                    }
  -- Operators
  \+                            { \_ -> TokenOperatorPlus                 }
  \-                            { \_ -> TokenOperatorMinus                }
  \*                            { \_ -> TokenOperatorTimes                }
  \/                            { \_ -> TokenOperatorDiv                  }
  \%                            { \_ -> TokenOperatorMod                  }
  \=\=                          { \_ -> TokenOperatorEq                   }
  \!\=                          { \_ -> TokenOperatorNeq                  }
  \<                            { \_ -> TokenOperatorLt                   }
  \<\=                          { \_ -> TokenOperatorLeq                  }
  \>                            { \_ -> TokenOperatorGt                   }
  \>\=                          { \_ -> TokenOperatorGeq                  }
  \&\&                          { \_ -> TokenOperatorAnd                  }
  \|\|                          { \_ -> TokenOperatorOr                   }
  \!                            { \_ -> TokenOperatorNot                  }
  \=                            { \_ -> TokenOperatorAssign               }
  \&                            { \_ -> TokenOperatorRef                  }
  \.                            { \_ -> TokenOperatorDot                  }
  \@                            { \_ -> TokenOperatorAt                   }
  -- Identifiers
  $alpha($alpha | $digit)*      { \s -> TokenIdentifier s                 }

{
scan :: String -> [Token]
scan = alexScanTokens
}