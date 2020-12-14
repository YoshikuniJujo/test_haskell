{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exponential.NoExponential where

import Control.Applicative
import Control.Arrow
import Data.Bool
import Data.List
import Data.Parse

parseExpr :: String -> Maybe Expression
parseExpr src = fst <$> expr (memo src)

data Expression = One | Expression :+ Expression deriving Show

data Memo = Memo {
	expr :: Maybe (Expression, Memo),
	term :: Maybe (Expression, Memo),
	char :: Maybe (Char, Memo) }

memo :: String -> Memo
memo cs = m where
	m = Memo ex tm ch
	ex = unparse pExpr m
	tm = unparse pTerm m
	ch = (memo `second`) <$> uncons cs

pExpr :: Parse Memo Expression
pExpr =	(:+) <$> parse term <* pick '+' <*> parse expr <|> parse term

pTerm :: Parse Memo Expression
pTerm = pick '(' *> parse expr <* pick ')' <|> One <$ pick '1'

pick :: Char -> Parse Memo Char
pick = check . (==)

check :: (Char -> Bool) -> Parse Memo Char
check p = parse char >>= \c -> bool empty (pure c) (p c)

sampleSrc' :: String
sampleSrc' = "((((((((((((1))))))))))))"
