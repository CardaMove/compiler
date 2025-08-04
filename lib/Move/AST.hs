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

-- | An address can either be named (string name) or numerical
data Address
  = NamedAddress Identifier
  | NumericalAddress Numerical
  deriving (Eq, Show)

-- | An identifier is a name of a variable or module
newtype Identifier
  = Identifier String
  deriving (Eq, Show)

-- | A integer value
data Numerical
  = LiteralIntDec Int
  | LiteralIntHex String
  deriving (Eq, Show)

-- | Represents any top level construct
data TopLevel
  = TopLevelUse Use
  | TopLevelFriend Friend
  | TopLevelNamedStruct NamedStruct
  | TopLevelPositionalStruct PositionalStruct
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
    namedStructAbilities :: [Ability],
    namedStructFields :: [NamedField]
  }
  deriving (Eq, Show)

data PositionalStruct
  = PositionalStruct {
    positionalStructIdentifier :: Identifier,
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
    fieldType :: TypeName
  }
  deriving (Eq, Show)

-- | Positional fields are field definitions inside a positional struct
newtype PositionalField = PositionalField TypeName
  deriving (Eq, Show)

-- | Represents a type
newtype TypeName
  = TypeName String
  deriving (Eq, Show)




{-
data Function = Function
  { functionName :: String,
    functionParameters :: [(String, String)],
    functionReturnType :: String,
    functionBody :: [Stmt]
  }
  deriving (Eq, Show)
-}

{-
data Constant = Constant
  { constantIdentifier :: String,
    constantType :: String,
    constantExpr :: Expr
  }
  deriving (Eq, Show)
-}

{-
newtype Identifier = Identifier String
  deriving (Eq, Show)

newtype Type = Type String
  deriving (Eq, Show)

data Use
  = Use Address Identifier
  deriving (Eq, Show)

data Constant
  = Constant Identifier Type Expr
  deriving (Eq, Show)

data Function
  = Function Identifier [(Identifier, Type)] Type [Stmt]
  deriving (Eq, Show)

data Expr
  = Var Identifier
  | Let Identifier Expr Expr
  deriving (Eq, Show)

newtype Stmt = Stmt Expr
  deriving (Eq, Show)

-- | friend <address>::<module>
data Friend
  = Friend String String
  deriving (Eq, Show)

-- | drop, copy, store, key

-- | struct <name> { <record: type>* } has <ability>
data Struct
  = Struct String [(String, String)] [Ability]
  deriving (Eq, Show)
-}