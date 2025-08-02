module Move.Token (Token (..)) where

data Token
  = -- Separators
    TokenSeparatorLParen
  | TokenSeparatorRParen
  | TokenSeparatorLBrace
  | TokenSeparatorRBrace
  | TokenSeparatorSemiColon
  | TokenSeparatorComma
  | TokenSeparatorColon
  | TokenSeparatorDoubleColon
  | -- Literals
    TokenLiteralIntDec Int
  | TokenLiteralIntHex String
  | TokenLiteralString String
  | TokenLiteralBool Bool
  | -- Keywords: Module, Script
    TokenKeywordConst
  | TokenKeywordFriend
  | TokenKeywordFun
  | TokenKeywordModule
  | TokenKeywordScript
  | TokenKeywordUse
  | -- Keywords: Structs
    TokenKeywordStruct
  | TokenKeywordHas
  | TokenKeywordKey
  | TokenKeywordStore
  | TokenKeywordDrop
  | TokenKeywordCopy
  | -- Keywords: Control flow
    TokenKeywordIf
  | TokenKeywordElse
  | TokenKeywordWhile
  | TokenKeywordLoop
  | TokenKeywordBreak
  | TokenKeywordContinue
  | -- Keywords: Let binding
    TokenKeywordLet
  -- | TokenKeywordIn
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
  | TokenOperatorRef
  | TokenOperatorRefMut
  | TokenOperatorDot
  | TokenOperatorDoubleDot
  | TokenOperatorAt
  | -- Identifiers
    TokenIdentifier String
  deriving (Eq, Show)