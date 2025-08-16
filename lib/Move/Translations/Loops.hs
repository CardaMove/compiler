module Move.Translations.Loops (mapLoopsToWhile) where

import Move.AST (Expr(..), While (..), ValueLiteral (Boolean))
-- Importing from Uniplate.Data allows to derive Biplate instances automatically from data types that derive Data
import Data.Generics.Uniplate.Data (transformBi)
import Data.Data (Data)

-- Need to declare that the function only works on data types deriving Data
mapLoopsToWhile :: Data from => from -> from
mapLoopsToWhile = transformBi f
    where
        f (Loop expr) = WhileTerm $ While {
            whileCondition = ValueLiteral $ Boolean True,
            whileExpr = expr
        }
        f expr = expr