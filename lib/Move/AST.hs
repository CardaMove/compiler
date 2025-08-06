module Move.AST where

-- | A module consists in an address, identifier and top level elements
data Module
  = Module {
    moduleAddress :: Address,
    moduleIdentifier :: Identifier,
    -- Top levels are ordered from top to bottom
    moduleTopLevels :: [TopLevel]
  }
  deriving (Eq, Show)


-- | An identifier is a name of a variable or module
newtype Identifier
  = Identifier String
  deriving (Eq, Show)


-- | Represents any top level construct
data TopLevel
  = TopLevelUse Use
  | TopLevelFriend Friend
  | TopLevelNamedStruct NamedStruct
  | TopLevelPositionalStruct PositionalStruct
  | TopLevelFunction Function
  | TopLevelConstant Constant
  deriving (Eq, Show)

-- | Using another module
data Use
  = Use {
    useAddress :: Address,
    useName :: Identifier,
    useAlias :: Maybe Identifier -- Alias is optional
  }
  deriving (Eq, Show)

-- | Friend with another module
data Friend
  = Friend {
    friendAddress :: Maybe Address, -- Address might be Nothing if the friend is aliased
    friendName :: Identifier
  }
  deriving (Eq, Show)

-- | Definition of a struct type
data NamedStruct
  = NamedStruct {
    namedStructIdentifier :: Identifier,
    namedStructTypeParameters :: [TypeParameter],
    namedStructAbilities :: [Ability],
    namedStructFields :: [NamedField]
  }
  deriving (Eq, Show)

-- | Positional struct
data PositionalStruct
  = PositionalStruct {
    positionalStructIdentifier :: Identifier,
    positionalStructTypeParameters :: [TypeParameter],
    positionalStructAbilities :: [Ability],
    positionalStructFields :: [PositionalField]
  }
  deriving (Eq, Show)

-- | Abilities of a struct
data Ability
  = Copy
  | Drop
  | Key
  | Store
  deriving (Eq, Show)

-- | Named fields are field definitions inside a "normal" (named) struct
data NamedField
  = NamedField {
    fieldIdentifier :: Identifier,
    fieldType :: Type
  }
  deriving (Eq, Show)

-- | Positional fields are field definitions inside a positional struct
newtype PositionalField = PositionalField Type
  deriving (Eq, Show)

-- | Represents a type, optionally with type parameters
data Type
  = Type Identifier [Type]
  deriving (Eq, Show)


-- | A function declaration
data Function
  = Function {
    functionHasNativeModifier :: Bool,
    functionVisibilityModifier :: Maybe VisibilityModifier,
    functionHasEntryModifier :: Bool,
    functionName :: Identifier,
    functionTypeParameters :: [TypeParameter],
    functionParameters :: [Parameter],
    functionReturnType :: Maybe Type,
    functionAcquires :: [Type],
    functionBody :: () -- TODO: function body
  }
  deriving (Eq, Show)

data VisibilityModifier
  = VisibilityModifierPublic
  | VisibilityModifierPackage
  | VisibilityModifierFriend
  deriving (Eq, Show)

data TypeParameter
  = TypeParameter {
    typeParameterIsPhantom :: Bool,
    typeIdentifier :: Identifier, -- Not using Type since here we have just an indentifier
    typeConstraints :: [Ability]
  }
  deriving (Eq, Show)

data Parameter
  = Parameter {
    parameterIdentifier :: Identifier,
    parameterType :: Type
  }
  deriving (Eq, Show)


data Constant
  = Constant {
    constantIdentifier :: Identifier,
    constantType :: Type,
    constantExpression :: Expr
  }
  deriving (Eq, Show)


--      Exp =
--            <LambdaBindList> <Exp>        spec only
--          | <Quantifier>                  spec only
--          | <BinOpExp>
--          | <UnaryExp> "=" <Exp>


--      BinOpExp =
--          <BinOpExp> <BinOp> <BinOpExp>
--          | <UnaryExp>
--      BinOp = (listed from lowest to highest precedence)
--          "==>"                                       spec only
--          | "||"
--          | "&&"
--          | "==" | "!=" | '<' | ">" | "<=" | ">="
--          | ".."                                      spec only
--          | "|"
--          | "^"
--          | "&"
--          | "<<" | ">>"
--          | "+" | "-"
--          | "*" | "/" | "%"


--      UnaryExp =
--          "!" <UnaryExp>
--          | "&mut" <UnaryExp>
--          | "&" <UnaryExp>
--          | "*" <UnaryExp>
--          | "move" <Var>
--          | "copy" <Var>
--          | <DotOrIndexChain>


--      DotOrIndexChain =
--          <DotOrIndexChain> "." <Identifier>
--          | <DotOrIndexChain> "[" <Exp> "]"                      spec only
--          | <Term>


--      Term =
--          "break"
--          | "continue"
--          | "vector" ('<' Comma<Type> ">")? "[" Comma<Exp> "]"
--          | <NameExp>               -- FIXME: TODO: Inferred from the Move compiler: represents a variable, a function call, a literal struct
--          | <Value>
--          | "(" Comma<Exp> ")"
--          | "(" <Exp> ":" <Type> ")"
--          | "(" <Exp> "as" <Type> ")"
--          | "{" <Sequence>
--          | "if" "(" <Exp> ")" <Exp> "else" "{" <Exp> "}"
--          | "if" "(" <Exp> ")" "{" <Exp> "}"
--          | "if" "(" <Exp> ")" <Exp> ("else" <Exp>)?
--          | "while" "(" <Exp> ")" "{" <Exp> "}"
--          | "while" "(" <Exp> ")" <Exp> (SpecBlock)?
--          | "loop" <Exp>
--          | "loop" "{" <Exp> "}"
--          | "return" "{" <Exp> "}"
--          | "return" <Exp>?
--          | "abort" "{" <Exp> "}"
--          | "abort" <Exp>


--      Value =
--          "@" <LeadingAccessName>
--          | "true"
--          | "false"
--          | <Number>
--          | <NumberTyped>
--          | <ByteString>


--      NameExp =
--          <NameAccessChain> <OptionalTypeArgs> "{" Comma<ExpField> "}"
--          | <NameAccessChain> <OptionalTypeArgs> "(" Comma<Exp> ")"
--          | <NameAccessChain> "!" "(" Comma<Exp> ")"
--          | <NameAccessChain> <OptionalTypeArgs>


--      ExpField = <Field> <":" <Exp>>?


--      NameAccessChain = <LeadingNameAccess> ( "::" <Identifier> ( "::" <Identifier> )? )?


-- TODO: rewrite se Identifier into NameAccessChain

-- | Represents any expression
data Expr
  = BinaryOpExprExpr BinaryOpExpr
  | AssignmentExpr Assignment
  | UnaryOpExpr UnaryExpr
  | DotOrIndexChainExpr DotOrIndexChain
  | ValueLiteral ValueLiteral
  | CommaExpr [Expr]
  | TypedExprTerm TypedExpr
  | CastingTerm Casting
  | NamedStructExprExpr NamedStructExpr
  -- TODO: | PositionalStructExprOrFunctionCallExpr PositionalStructExprOrFunctionCall
  | IfThenElseTerm IfThenElse
  | WhileTerm While
  | Loop Expr
  | Return (Maybe Expr)
  | Abort Expr
  | Break
  | Continue
  deriving (Eq, Show)

data BinaryOpExpr
  = Or Expr Expr
  | And Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Leq Expr Expr
  | Geq Expr Expr
  | BitwiseOr Expr Expr
  | BitwiseXor Expr Expr
  | BitwiseAnd Expr Expr
  | ShiftLeft Expr Expr
  | ShiftRight Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  deriving (Eq, Show)


data Assignment
  = Assignment {
    assignmentLeft :: Expr,
    assignmentRight :: Expr
  }
  deriving (Eq, Show)


data UnaryExpr
  = Negation Expr
  | MutableReference Expr
  | ImmutableReference Expr
  | Dereference Expr
  | MoveExpr Identifier
  | CopyExpr Identifier
  deriving (Eq, Show)


data DotOrIndexChain
  = DotAccess {
    dotAccessLeft :: Expr,
    dotAccessRight :: Identifier
  }
  deriving (Eq, Show)


data While
  = While {
    whileCondition :: Expr,
    whileExpr :: Expr
  }
  deriving (Eq, Show)


data IfThenElse
  = IfThenElse {
    ifThenElseCondition :: Expr,
    ifThenElseIfBranch :: Expr,
    ifThenElseElseBranch :: Maybe Expr
  }
  deriving (Eq, Show)


data Casting
  = Casting {
    castingExpr :: Expr,
    castingType :: Type
  }
  deriving (Eq, Show)


data TypedExpr
  = TypedExpr {
    typedExpr :: Expr,
    typedExprType :: Type
  }
  deriving (Eq, Show)


data ValueLiteral
  = Address Address
  | Boolean Bool
  | Numerical Numerical
  deriving (Eq, Show)


-- | An address can either be named (string name) or numerical
data Address
  = NamedAddress Identifier
  | NumericalAddress Numerical
  deriving (Eq, Show)


-- | A integer value
data Numerical
  = LiteralIntDec Int
  | LiteralIntHex String
  deriving (Eq, Show)


data NamedStructExpr
  = NamedStructExpr {
    nseNameAccessChain :: NameAccessChain,
    nseTypeArgs :: [Type],
    nseFields :: [NamedStructExprField]
  }
  deriving (Eq, Show)


data NamedStructExprField
  = NamedStructExprField {
    nsefIdentifier :: Identifier,
    nsefExpr :: Maybe Expr
  }
  deriving (Eq, Show)


data NameAccessChain
  = LocalNameAccessChain Identifier
  | AliasedNameAccessChain Address Identifier
  | UnaliasedNameAccessChain Address Identifier Identifier
  deriving (Eq, Show)
