module Translator (translate) where

import Aiken.AST qualified as Aiken
import Move.AST qualified as Move

class Translate m a where
  translate :: m -> a

instance Translate Move.Module Aiken.Module where
  translate m =
    Aiken.Module
      { Aiken.moduleTopLevels = map translate $ Move.moduleTopLevels m
      }

instance Translate Move.TopLevel Aiken.TopLevel where
  translate (Move.TopLevelStruct s) = Aiken.TopLevelTypeDefinition td
    where
      td = translate s

instance Translate Move.Struct Aiken.TypeDefinition where
  translate s =
    Aiken.TypeDefinition
      { Aiken.typeDefinitionIdentifier = Move.structIdentifier s,
        Aiken.typeDefinitionDataConstructors =
          [ Aiken.DataConstructor
              { Aiken.dataConstructorIdentifier = Move.structIdentifier s,
                Aiken.dataConstructorFields = map translate $ Move.structFields s
              }
          ]
      }

instance Translate Move.Field Aiken.Field where
  translate f =
    Aiken.Field
      { Aiken.fieldIdentifier = Move.fieldIdentifier f,
        Aiken.fieldType = Move.fieldType f
      }
