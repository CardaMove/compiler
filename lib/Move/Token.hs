module Move.Token (Token (..)) where

data Token
  = -- The whitespace is required to distinguish "less than" from "type arguments"
    -- For more details, see the NameExpr comment on `Parser.y`
    TokenWhitespace
  | -- Separators
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
  -- The following operator means a whitespace followed by a '<'.
  -- For more details, see the NameExpr comment on `Parser.y`
  | TokenOperatorWhiteLt
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
  -- See Parser.y about '>>' operator
  -- TokenOperatorShiftRight
  | -- Identifiers
    TokenIdentifier String
  deriving (Eq, Show)