{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exponential.Exponential where

import Control.Applicative
import Data.Bool
import Data.Parse

parseChar :: Parse String Char
parseChar = Parse \case "" -> Nothing; (c : cs) -> Just (c, cs)

parseEnd :: Parse String ()
parseEnd = Parse \case "" -> Just ((), ""); _ -> Nothing

succeed :: Parse String ()
succeed = Parse \s -> Just ((), s)

check :: (Char -> Bool) -> Parse String Char
check p = parseChar >>= \c -> bool empty (pure c) (p c)

pick :: Char -> Parse String Char
pick = check . (==)

data Expression
	= One
	| Expression :+ Expression
	| Expression :- Expression
	deriving Show

type Op = Expression -> Expression -> Expression

parseOpen :: Parse String ()
parseOpen = () <$ pick '('

parseClose :: Parse String ()
parseClose = () <$ pick ')'

parseAdd, parseSub :: Parse String Op
parseAdd = (:+) <$ pick '+'
parseSub = (:-) <$ pick '-'

parseOne :: Parse String Expression
parseOne = One <$ pick '1'

parseA :: Parse String Expression
parseA =
	(:+) <$> parseP <* parseAdd <*> parseA <|>
	(:-) <$> parseP <* parseSub <*> parseA <|>
	parseP

parseP :: Parse String Expression
parseP = parseOpen *> parseA <* parseClose <|> parseOne

sampleSrc :: String
sampleSrc = "((((((((((((1))))))))))))"
