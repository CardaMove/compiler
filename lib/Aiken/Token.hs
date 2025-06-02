module Aiken.Token (Token (..)) where

data Token
  = -- Separators
    TokenSeparatorLParen
  | TokenSeparatorRParen
  | TokenSeparatorLBrace
  | TokenSeparatorRBrace
  | TokenSeparatorComma
  | TokenSeparatorColon
  | TokenSeparatorSlash
  | -- Literals
    TokenLiteralIntDec Int
  | TokenLiteralString String
  | TokenLiteralBool Bool
  | -- Keywords
    TokenKeywordFn
  | TokenKeywordUse
  | TokenKeywordType
  | TokenKeywordPub
  | TokenKeywordIf
  | TokenKeywordThen
  | TokenKeywordElse
  | TokenKeywordLet
  | TokenKeywordIn
  | -- Operators
    TokenOperatorPlus
  | TokenOperatorMinus
  | TokenOperatorTimes
  | TokenOperatorDiv
  | TokenOperatorMod
  | TokenOperatorEq
  | TokenOperatorNeq
  | TokenOperatorLt
  | TokenOperatorLeq
  | TokenOperatorGt
  | TokenOperatorGeq
  | TokenOperatorAnd
  | TokenOperatorOr
  | TokenOperatorNot
  | TokenOperatorAssign
  | TokenOperatorDot
  | TokenOperatorDDot
  | TokenOperatorBackArrow
  | TokenOperatorArrow
  | -- Identifiers
    TokenIdentifier String
  deriving (Show, Eq)