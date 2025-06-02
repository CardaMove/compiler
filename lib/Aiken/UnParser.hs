module Aiken.UnParser (unParse) where

import Aiken.AST
import Aiken.Token
import Data.List (intercalate)

class UnParse t where
  unParse :: t -> [Token]

instance UnParse Module where
  unParse (Module topLevel) = concatMap unParse topLevel

instance UnParse TopLevel where
  unParse (TopLevelTypeDefinition typeDefinition) = unParse typeDefinition

instance UnParse TypeDefinition where
  unParse (TypeDefinition identifier dataConstructors) =
    [TokenKeywordType, TokenIdentifier identifier, TokenSeparatorLBrace] ++ concatMap unParse dataConstructors ++ [TokenSeparatorRBrace]

instance UnParse DataConstructor where
  unParse (DataConstructor identifier fields) =
    [TokenIdentifier identifier, TokenSeparatorLBrace] ++ unParse fields ++ [TokenSeparatorRBrace]

instance UnParse [Field] where
  unParse fields = intercalate [TokenSeparatorComma] $ map unParse fields

instance UnParse Field where
  unParse (Field identifier fieldType) = [TokenIdentifier identifier, TokenSeparatorColon, TokenIdentifier fieldType]