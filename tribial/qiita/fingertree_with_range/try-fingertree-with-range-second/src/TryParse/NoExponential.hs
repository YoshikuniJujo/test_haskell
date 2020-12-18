{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryParse.NoExponential where

import Control.Applicative
import Control.Arrow
import Data.Bool
import Data.List
import Data.Parse

data Expression = One | Expression :+ Expression deriving Show

data Memo = Memo {
	expr :: Maybe (Expression, Memo),
	term :: Maybe (Expression, Memo),
	char :: Maybe (Char, Memo) }

check :: (Char -> Bool) -> Parse Memo Char
check p = parse char >>= \c -> bool empty (pure c) (p c)

pick :: Char -> Parse Memo Char
pick = check . (==)

pExpr, pTerm :: Parse Memo Expression
pExpr = (:+) <$> parse term <* pick '+' <*> parse expr <|> parse term
pTerm = pick '(' *> parse expr <* pick ')' <|> One <$ pick '1'

memo :: String -> Memo
memo cs = m where
	m = Memo ex tm ch
	ex = unparse pExpr m
	tm = unparse pTerm m
	ch = (memo `second`) <$> uncons cs

parseExpr :: String -> Maybe Expression
parseExpr src = fst <$> expr (memo src)
