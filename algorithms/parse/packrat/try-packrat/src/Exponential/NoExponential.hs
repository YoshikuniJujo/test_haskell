{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exponential.NoExponential where

import Control.Applicative
import Control.Arrow
import Data.Bool
import Data.List
import Data.Parse

data Expression = One | Expression :+ Expression | Expression :- Expression
	deriving Show

type Op = Expression -> Expression -> Expression

data Memo = Memo {
	expr :: Maybe (Expression, Memo),
	term :: Maybe (Expression, Memo),
	open :: Maybe ((), Memo),
	close :: Maybe ((), Memo),
	add :: Maybe (Op, Memo),
	sub :: Maybe (Op, Memo),
	one :: Maybe (Expression, Memo),
	char :: Maybe (Char, Memo) }

memo :: String -> Memo
memo cs = m where
	m = Memo ex tm op cl ad sb on ch
	ex = unparse pExpr m
	tm = unparse pTerm m
	op = unparse pOpen m
	cl = unparse pClose m
	ad = unparse pAdd m
	sb = unparse pSub m
	on = unparse pOne m
	ch = (memo `second`) <$> uncons cs

parseIt :: (Memo -> Maybe (a, Memo)) -> String -> Maybe a
parseIt nts src = fst <$> nts (memo src)

pExpr :: Parse Memo Expression
pExpr =	(:+) <$> parse term <* parse add <*> parse expr <|>
	(:-) <$> parse term <* parse sub <*> parse expr <|>
	parse term

pTerm :: Parse Memo Expression
pTerm = parse open *> parse expr <* parse close <|> parse one

pOpen :: Parse Memo ()
pOpen = () <$ pick '('

pClose :: Parse Memo ()
pClose = () <$ pick ')'

pAdd, pSub :: Parse Memo Op
pAdd = (:+) <$ pick '+'
pSub = (:-) <$ pick '-'

pOne :: Parse Memo Expression
pOne = One <$ pick '1'

pick :: Char -> Parse Memo Char
pick = check . (==)

check :: (Char -> Bool) -> Parse Memo Char
check p = parse char >>= \c -> bool empty (pure c) (p c)

sampleSrc' :: String
sampleSrc' = "((((((((((((1))))))))))))"
