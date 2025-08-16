module Move.Token (Token (..)) where

data Token
  = -- Separators
    TokenSeparatorLParen
  | TokenSeparatorRParen
  | TokenSeparatorLBrace
  | TokenSeparatorRBrace
  | TokenSeparatorLSquareBracket
  | TokenSeparatorRSquareBracket
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
  | -- Keywords: Functions
    TokenKeywordPublic
  | TokenKeywordPackage
  | TokenKeywordEntry
  | TokenKeywordAcquires
  | TokenKeywordNative
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
  | TokenKeywordFor
  | TokenKeywordLoop
  | TokenKeywordBreak
  | TokenKeywordContinue
  | TokenKeywordReturn
  | TokenKeywordAbort
  | -- Other keywords
    TokenKeywordLet
  | TokenKeywordPhantom
  | TokenKeywordAs
  | TokenKeywordMove
  | -- Operators

    -- | TokenKeywordIn
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
  | TokenOperatorAmp
  | TokenOperatorBitwiseOr
  | TokenOperatorBitwiseXor
  | TokenOperatorAmpMut
  | TokenOperatorDot
  | TokenOperatorDoubleDot
  | TokenOperatorAt
  | TokenOperatorShiftLeft
  | TokenOperatorShiftRight
  | -- Identifiers
    TokenIdentifier String
  deriving (Eq, Show)