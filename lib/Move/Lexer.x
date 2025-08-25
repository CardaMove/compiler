{
module Move.Lexer (scan) where

import Move.Token
}

%wrapper "posn"

-- Macros
$digit                          = [0-9]
$alpha                          = [a-zA-Z]
$hex                            = [0-9a-fA-F]
$graphic                        = $printable # $white
@string                         = \" ($graphic # \")* \"
@literalIntType                 = u8 | u16 | u32 | u64 | u128 | u256    -- Type suffix for integer literals

-- Rules
tokens :-
  -- Ignored tokens
  -- Note that whitespace must be considered in a single case
  -- For more details, see the NameExpr comment on `Parser.y`
  $white+                       { \_ _ -> TokenWhitespace                   }
  "//".*                        ; -- skip comments
  -- Separators
  \(                            { \_ _ -> TokenSeparatorLParen              }
  \)                            { \_ _ -> TokenSeparatorRParen              }
  \{                            { \_ _ -> TokenSeparatorLBrace              }
  \}                            { \_ _ -> TokenSeparatorRBrace              }
  \[                            { \_ _ -> TokenSeparatorLSquareBracket      }
  \]                            { \_ _ -> TokenSeparatorRSquareBracket      }
  \,                            { \_ _ -> TokenSeparatorComma               }
  \:                            { \_ _ -> TokenSeparatorColon               }
  \;                            { \_ _ -> TokenSeparatorSemiColon           }
  \:\:                          { \_ _ -> TokenSeparatorDoubleColon         }
  -- Literals
  -- Integers (both digits and hex literals) can have underscores and suffix, that need to be ignored
  $digit($digit | _)*(@literalIntType)?                     { \_ s -> TokenLiteralIntDec $ read $ parseLiteralInteger s       }
  0x$hex($hex | _)*(@literalIntType)?                       { \_ s -> TokenLiteralIntHex $ parseLiteralInteger s              }
  @string                       { \_ s -> TokenLiteralString s              }
  true                          { \_ _ -> TokenLiteralBool True             } 
  false                         { \_ _ -> TokenLiteralBool False            }
  -- Keywords: Module, Script
  const                         { \_ _ -> TokenKeywordConst                 }
  friend                        { \_ _ -> TokenKeywordFriend                }
  fun                           { \_ _ -> TokenKeywordFun                   }
  module                        { \_ _ -> TokenKeywordModule                }
  script                        { \_ _ -> TokenKeywordScript                }
  use                           { \_ _ -> TokenKeywordUse                   }
  -- Keywords: Functions
  public                        { \_ _ -> TokenKeywordPublic                }
  package                       { \_ _ -> TokenKeywordPackage               }
  entry                         { \_ _ -> TokenKeywordEntry                 }
  acquires                      { \_ _ -> TokenKeywordAcquires              }
  native                        { \_ _ -> TokenKeywordNative                }
  -- TODO: Inline functions and lambdas
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
  for                           { \_ _ -> TokenKeywordFor                   }
  loop                          { \_ _ -> TokenKeywordLoop                  }
  break                         { \_ _ -> TokenKeywordBreak                 }
  continue                      { \_ _ -> TokenKeywordContinue              }
  return                        { \_ _ -> TokenKeywordReturn                }
  abort                         { \_ _ -> TokenKeywordAbort                 }
  -- Other keywords
  let                           { \_ _ -> TokenKeywordLet                   }
  phantom                       { \_ _ -> TokenKeywordPhantom               }
  as                            { \_ _ -> TokenKeywordAs                    }
  move                          { \_ _ -> TokenKeywordMove                  }
  -- in                            { \_ _ -> TokenKeywordIn                    }
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
  \&                            { \_ _ -> TokenOperatorAmp                  }
  \|                            { \_ _ -> TokenOperatorBitwiseOr            }
  \^                            { \_ _ -> TokenOperatorBitwiseXor           }
  \&mut                         { \_ _ -> TokenOperatorAmpMut               }
  \.                            { \_ _ -> TokenOperatorDot                  }
  \.\.                          { \_ _ -> TokenOperatorDoubleDot            }
  \@                            { \_ _ -> TokenOperatorAt                   }
  \<\<                          { \_ _ -> TokenOperatorShiftLeft            }
  \>\>                          { \_ _ -> TokenOperatorShiftRight           }
  -- TODO: Compound assignments
  -- TODO: vector<u8> literarls with b"" and x""
  -- Identifiers
  -- Generic identifier syntax for variable, module and structs names
  (_ | $alpha)(_ | $alpha | $digit)*      { \_ s -> TokenIdentifier s                 }

{
scan :: String -> [Token]
scan str = filterWhitespace $ alexScanTokens str

-- | Removes underscores and suffix from an integer literal
parseLiteralInteger :: String -> String
parseLiteralInteger = filter (/= '_') . takeWhile (/= 'u')

-- |
-- Given a list of tokens, if there is a whitespace token followed by a '<',
-- replaces them with a single `TokenOperatorWhiteLt` token,
-- otherwise just removes the whitespace token.
--
-- This is used to prevent an ambiguity of the grammar.
-- For more details, see the NameExpr comment on `Parser.y`
filterWhitespace :: [Token] -> [Token]
filterWhitespace [] = []
filterWhitespace (TokenWhitespace:TokenOperatorLt:toks) = TokenOperatorWhiteLt:filterWhitespace toks
filterWhitespace (TokenWhitespace:toks) = filterWhitespace toks
filterWhitespace (token:toks) = token:filterWhitespace toks
}