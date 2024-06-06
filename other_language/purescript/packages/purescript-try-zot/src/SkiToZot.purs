module SkiToZot where

import Control.Monad
import Data.Semigroup
import Data.Enum
import Data.Function
import Data.Maybe
import Data.List
import Data.String

skiToZot :: String -> Maybe String
skiToZot =
        skiToZotGen <<< (fromEnum <$> _) <<< fromFoldable <<< toCodePointArray

skiToZotGen :: List Int -> Maybe String
skiToZotGen Nil = Just ""
skiToZotGen (0x0a : cs) = skiToZotGen cs
skiToZotGen (0x60 : cs) = ("1" <> _) <$> skiToZotGen cs
skiToZotGen (0x69 : cs) = ("100" <> _) <$> skiToZotGen cs
skiToZotGen (0x6b : cs) = ("1010100" <> _) <$> skiToZotGen cs
skiToZotGen (0x73 : cs) = ("101010100" <> _) <$> skiToZotGen cs
skiToZotGen _ = Nothing

-- \n   `       i       k       s
-- 0x0a 0x60    0x69    0x6b    0x73
