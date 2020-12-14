{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Derivation.Parse where

import Control.Applicative (empty, many, (<|>))
import Control.Arrow
import Data.Function
import Data.Maybe
import Data.List
import Data.Char

import Data.Parse
import Data.Derivation.Expression

import qualified Data.Bool as B

data Memo = Memo {
	constraint :: Maybe (Exp Var Bool, Memo),
	equal :: Maybe (Exp Var Bool, Memo),
	bool :: Maybe (Exp Var Bool, Memo),
	lessEqual :: Maybe (Exp Var Bool, Memo),
	polynomial :: Maybe (Exp Var Number, Memo),
	number :: Maybe (Exp Var Number, Memo),
	token :: Maybe (String, Memo) }

type Var = String

check :: (String -> Bool) -> Parse Memo String
check p = parse token >>= \t -> B.bool empty (pure t) (p t)

pick :: String -> Parse Memo String
pick = check . (==)

pPolynomial :: Parse Memo (Exp Var Number)
pPolynomial = foldl (&) <$> parse number <*> many (
	flip (:+) <$> (pick "+" *> parse number) <|>
	flip (:-) <$> (pick "-" *> parse number) )

pNumber :: Parse Memo (Exp Var Number)
pNumber =
	Const . read <$> check (all isDigit) <|> Var <$> check (all isLower) <|>
	pick "(" *> parse polynomial <* pick ")"

memo :: [String] -> Memo
memo ts = undefined
