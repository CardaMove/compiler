module Move.Translations.Loops (mapLoopsToWhile) where

-- Importing from Uniplate.Data allows to derive Biplate instances automatically from data types that derive Data

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBi)
import Move.AST (Expr (..), ValueLiteral (Boolean), While (..))

-- |
-- Translates all `loop expr` expressions into `while(true) expr`.
--
-- Works recursively on any node of the AST
mapLoopsToWhile :: (Data from) => from -> from
mapLoopsToWhile = transformBi f
  where
    f (Loop expr) =
      WhileTerm $
        While
          { whileCondition = ValueLiteral $ Boolean True,
            whileExpr = expr
          }
    f expr = expr