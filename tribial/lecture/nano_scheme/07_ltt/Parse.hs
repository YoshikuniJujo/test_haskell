module Parse (Token, tokens, parse) where

import Data.Char

import Environment
import Maybe

data Token
	= TkSym Symbol
	| TkBool Bool
	| TkInt Integer
	| TkOPr
	| TkCPr
	deriving Show

tokens :: String -> Maybe [Token]
tokens ('(' : s) = (TkOPr :) `mapply` tokens s
tokens (')' : s) = (TkCPr :) `mapply` tokens s
tokens ('#' : 'f' : s) = (TkBool False :) `mapply` tokens s
tokens ('#' : 't' : s) = (TkBool True :) `mapply` tokens s
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
parse1 (TkBool b : ts) = Just (Bool b, ts)
parse1 (TkInt i : ts) = Just (Int i, ts)
parse1 (TkOPr : ts) = (\(vs, ts') -> (List vs, ts')) `mapply` parseL ts
parse1 _ = Nothing

parseL :: [Token] -> Maybe ([Value], [Token])
parseL (TkCPr : ts) = Just ([], ts)
parseL ts = case parse1 ts of
	Just (v, ts') -> (\(vs, ts'') -> (v : vs, ts'')) `mapply` parseL ts'
	_ -> Nothing
