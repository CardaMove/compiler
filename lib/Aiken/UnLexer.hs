module Aiken.UnLexer (unLex) where

import Aiken.Token

class UnLex t where
  unLex :: t -> String

instance UnLex [Token] where
  unLex tokens = unwords (map unLex tokens)

instance UnLex Token where
  unLex TokenSeparatorLParen = "("
  unLex TokenSeparatorRParen = ")"
  unLex TokenSeparatorLBrace = "{"
  unLex TokenSeparatorRBrace = "}"
  unLex TokenSeparatorComma = ","
  unLex TokenSeparatorColon = ":"
  unLex TokenKeywordType = "type"
  unLex (TokenIdentifier s) = s
  unLex _ = error "UnLex: unimplemented"