module Aiken.AST
  ( DataConstructor (..),
    Field (..),
    Module (..),
    TopLevel (..),
    TypeDefinition (..),
  )
where

newtype Module
  = Module
  { moduleTopLevels :: [TopLevel]
  }
  deriving (Eq, Show)

newtype TopLevel
  = TopLevelTypeDefinition TypeDefinition
  deriving (Eq, Show)

data TypeDefinition
  = TypeDefinition
  { typeDefinitionIdentifier :: String,
    typeDefinitionDataConstructors :: [DataConstructor]
  }
  deriving (Eq, Show)

data DataConstructor
  = DataConstructor
  { dataConstructorIdentifier :: String,
    dataConstructorFields :: [Field]
  }
  deriving (Eq, Show)

data Field
  = Field
  { fieldIdentifier :: String,
    fieldType :: String
  }
  deriving (Eq, Show)
