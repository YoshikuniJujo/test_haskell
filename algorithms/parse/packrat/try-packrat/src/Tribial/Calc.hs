{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tribial.Calc where

import Control.Applicative
import Data.Function
import Data.Bool
import Data.Maybe
import Data.List
import Data.Char

import Data.Parse

data Derivs = Derivs {
	expr :: Maybe (Integer, Derivs),
	term :: Maybe (Integer, Derivs),
	num :: Maybe (Integer, Derivs),
	token :: Maybe (String, Derivs) }

calc :: String -> Maybe Integer
calc s = fst <$> expr (derivs $ tokens s)

tokens :: String -> [String]
tokens = unfoldr (listToMaybe . lex)

derivs :: [String] -> Derivs
derivs ts = d where
	d = Derivs e tm n tk
	e = pExp d
	tm = pTerm d
	n = pNum d
	tk = case ts of
		(t : ts') -> Just (t, derivs ts')
		_ -> Nothing

check :: (String -> Bool) -> Parse Derivs String
check p = do
	t <- parse token
	bool empty (pure t) (p t)

tkn :: String -> Parse Derivs String
tkn s = do
	t <- parse token
	bool empty (pure t) (s == t)

pExp :: Derivs -> Maybe (Integer, Derivs)
pExp = unparse $ (\i is -> foldl (&) i is) <$> parse term
	<*> many (
		((+) <$> (tkn "+" *> parse term)) <|>
		(subtract <$> (tkn "-" *> parse term)) )

pTerm :: Derivs -> Maybe (Integer, Derivs)
pTerm = unparse $ (\i is -> foldl (&) i is) <$> parse num
	<*> many (
		((*) <$> (tkn "*" *> parse num)) <|>
		(flip div <$> (tkn "/" *> parse num)) )

pNum :: Derivs -> Maybe (Integer, Derivs)
pNum = unparse $ read <$> check (all isDigit)
