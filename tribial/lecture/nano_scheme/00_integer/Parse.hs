module Parse (Token, tokens, parse) where

import Data.Char

import Environment
import Maybe

data Token
	= TkInt Integer
	deriving Show

tokens :: String -> Maybe [Token]
tokens (c : s)
	| isDigit c = let (t, r) = span isDigit s in
		(TkInt (read $ c : t) :) `mapply` tokens r
	| isSpace c = tokens s
tokens "" = Just []
tokens _ = Nothing

parse :: [Token] -> Maybe [Value]
parse [] = Just []
parse ts = case parse1 ts of
	Just (v, ts') -> (v :) `mapply` parse ts'
	_ -> Nothing

parse1 :: [Token] -> Maybe (Value, [Token])
parse1 (TkInt i : ts) = Just (Int i, ts)
parse1 _ = Nothing
