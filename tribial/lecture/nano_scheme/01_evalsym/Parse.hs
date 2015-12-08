module Parse (Token, tokens, parse) where

import Data.Char

import Environment
import Maybe

data Token
	= TkSym Symbol
	| TkInt Integer
	deriving Show

tokens :: String -> Maybe [Token]
tokens (c : s)
	| isDigit c = let (t, r) = span isDigit s in
		(TkInt (read $ c : t) :) `mapply` tokens r
	| isSym c = let (t, r) = span isSym s in
		(TkSym (c : t) :) `mapply` tokens r
	| isSpace c = tokens s
tokens "" = Just []
tokens _ = Nothing

isSym :: Char -> Bool
isSym c = any ($ c) [isAlphaNum, (`elem` "+-*/<=>?")]

parse :: [Token] -> Maybe [Value]
parse [] = Just []
parse ts = case parse1 ts of
	Just (v, ts') -> (v :) `mapply` parse ts'
	_ -> Nothing

parse1 :: [Token] -> Maybe (Value, [Token])
parse1 (TkSym s : ts) = Just (Sym s, ts)
parse1 (TkInt i : ts) = Just (Int i, ts)
parse1 [] = Nothing
