{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Exponential.NoExponential where

import Control.Applicative
import Data.Bool
import Data.Parse

data Expression
	= One
	| Expression :+ Expression
	| Expression :- Expression
	deriving Show

type Op = Expression -> Expression -> Expression

data Memo = Memo {
	aa :: Maybe (Expression, Memo),
	pp :: Maybe (Expression, Memo),
	open :: Maybe ((), Memo),
	close :: Maybe ((), Memo),
	one :: Maybe (Expression, Memo),
	add :: Maybe (Op, Memo),
	sub :: Maybe (Op, Memo),
	char :: Maybe (Char, Memo) }

memo :: String -> Memo
memo cs = m where
	m = Memo a p op cl on ad sb ch
	a = runParse pA m
	p = runParse pP m
	op = runParse pOpen m
	cl = runParse pClose m
	on = runParse pOne m
	ad = runParse pAdd m
	sb = runParse pSub m
	ch = case cs of
		c : cs' -> Just (c, memo cs')
		_ -> Nothing

check :: (Char -> Bool) -> Parse Memo Char
check p = parse char >>= \c -> bool empty (pure c) (p c)

pick :: Char -> Parse Memo Char
pick = check . (==)

pA :: Parse Memo Expression
pA =	(:+) <$> parse pp <* parse add <*> parse aa <|>
	(:-) <$> parse pp <* parse sub <*> parse aa <|>
	parse pp

pP :: Parse Memo Expression
pP = parse open *> parse aa <* parse close <|> parse one

pOpen :: Parse Memo ()
pOpen = () <$ pick '('

pClose :: Parse Memo ()
pClose = () <$ pick ')'

pAdd, pSub :: Parse Memo Op
pAdd = (:+) <$ pick '+'
pSub = (:-) <$ pick '-'

pOne :: Parse Memo Expression
pOne = One <$ pick '1'

sampleSrc' :: String
sampleSrc' = "((((((((((((1))))))))))))"
