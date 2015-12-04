module Parse (Token, tokens, parse) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import Data.Char (isDigit, isAlpha, isSpace)

import Environment(Value(..), Symbol)

data Token = TkSym Symbol | TkInt Integer | TkFls | TkTru | TkOPr | TkCPr
	deriving Show

tokens :: String -> Maybe [Token]
tokens ('(' : s) = (TkOPr :) <$> tokens s
tokens (')' : s) = (TkCPr :) <$> tokens s
tokens ('#' : 'f' : s) = (TkFls :) <$> tokens s
tokens ('#' : 't' : s) = (TkTru :) <$> tokens s
tokens (c : s)
	| isSymCh c, let (t, r) = span isSymCh s = (TkSym (c : t) :) <$> tokens r
	| isDigit c, let (t, r) = span isDigit s = (tkInt (c : t) :) <$> tokens r
	| isSpace c = tokens s | otherwise = Nothing
	where
	isSymCh = (`any` [isAlpha, (`elem` "+-*/<=>?")]) . flip ($)
	tkInt = TkInt . read
tokens _ = Just []

parse :: [Token] -> Maybe [Value]
parse [] = Just []; parse ts = uncurry (<$>) . ((:) *** parse) =<< parse1 ts

parse1, parseCns :: [Token] -> Maybe (Value, [Token])
parse1 (TkSym s : ts) = Just (Symbol s, ts)
parse1 (TkInt i : ts) = Just (Int i, ts)
parse1 (TkFls : ts) = Just (B False, ts)
parse1 (TkTru : ts) = Just (B True, ts)
parse1 (TkOPr : ts) = parseCns ts
parse1 _ = Nothing

parseCns (TkCPr : ts) = Just (Nil, ts)
parseCns ts = uncurry (<$>) . (first . Cons *** parseCns) =<< parse1 ts
