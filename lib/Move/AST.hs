module Move.AST
  ( -- Ability (..),
    -- Field (..),
    Module (..),
    -- Struct (..),
    -- TopLevel (..),
    Address (..),
    Identifier(..),
    Numerical(..),
    -- TypeName(..)
  )
where

-- | A module consists in an address, identifier and top level elements
data Module
  = Module
  { moduleAddress :: Address,
    moduleIdentifier :: Identifier -- ,
    -- moduleTopLevels :: [TopLevel]
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


{- newtype TopLevel
  = TopLevelStruct Struct
  deriving (Eq, Show)

data Struct
  = Struct
  { structIdentifier :: Identifier,
    structAbilities :: [Ability],
    structFields :: [Field]
  }
  deriving (Eq, Show)

data Ability
  = Copy
  | Drop
  | Key
  | Store
  deriving (Eq, Show)

data Field
  = Field
  { fieldIdentifier :: Identifier,
    fieldType :: TypeName
  }
  deriving (Eq, Show)

newtype TypeName
  = TypeName String
  deriving (Eq, Show)

 -}
{-
data Use = Use
  { useAddress :: String,
    useIdentifier :: String
  }
  deriving (Eq, Show)

data Friend = Friend
  { friendAddress :: String,
    friendModule :: String
  }
  deriving (Eq, Show)
-}

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