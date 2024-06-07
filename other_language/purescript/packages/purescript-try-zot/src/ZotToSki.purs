module ZotToSki where

import Prelude
import Control.Monad
import Control.Alt
import Data.Semigroup
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Unit
import Data.String

import Parser

zotToSki :: String -> Maybe String
zotToSki s = case parseZot unit `runParser` fromFoldable (toCodePointArray s) of
        Just (Tuple r Nil) -> Just r
        _ -> Nothing

parseZot :: Unit -> Parser CodePoint String
parseZot _ = parseApply `alt` parseS `alt` parseK `alt` parseI

parseApply :: Parser CodePoint String
parseApply = do
        _ <- char '1'
        (\x y -> "`" <> x <> y) <$> parseZot unit <*> parseZot unit

parseS :: Parser CodePoint String
parseS = "s" <$ string "101010100"

parseK :: Parser CodePoint String
parseK = "k" <$ string "1010100"

parseI :: Parser CodePoint String
parseI = "i" <$ string "100"
