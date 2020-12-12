{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exponential.Exponential where

import Control.Applicative
import Data.Bool
import Data.Parse

data Expression = One | Expression :+ Expression | Expression :- Expression
	deriving Show

type Op = Expression -> Expression -> Expression

pExpr :: Parse String Expression
pExpr =	(:+) <$> pTerm <* pAdd <*> pExpr <|>
	(:-) <$> pTerm <* pSub <*> pExpr <|>
	pTerm

pTerm :: Parse String Expression
pTerm = pOpen *> pExpr <* pClose <|> pOne

pOpen :: Parse String ()
pOpen = () <$ pick '('

pClose :: Parse String ()
pClose = () <$ pick ')'

pAdd, pSub :: Parse String Op
pAdd = (:+) <$ pick '+'
pSub = (:-) <$ pick '-'

pOne :: Parse String Expression
pOne = One <$ pick '1'

pChar :: Parse String Char
pChar = Parse \case "" -> Nothing; (c : cs) -> Just (c, cs)

succeed :: Parse String ()
succeed = Parse \s -> Just ((), s)

pick :: Char -> Parse String Char
pick = check . (==)

check :: (Char -> Bool) -> Parse String Char
check p = pChar >>= \c -> bool empty (pure c) (p c)

sampleSrc :: String
sampleSrc = "((((((((((((1))))))))))))"
