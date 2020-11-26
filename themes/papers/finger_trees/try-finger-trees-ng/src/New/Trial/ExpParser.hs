{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Trial.ExpParser where

import Data.Maybe
import Data.List
import Data.Char

import New.Expression

parseBool :: [String] -> Maybe (Exp String Bool, [String])
parseBool ("F" : ts) = Just (Bool False, ts)
parseBool ("T" : ts) = Just (Bool True, ts)
parseBool ("(" : ts) = case parseLeq ts of
	Just r -> Just r
	Nothing -> parseEq ts
parseBool _ = Nothing

parseLeq :: [String] -> Maybe (Exp String Bool, [String])
parseLeq ts = case parseTerm ts of
	Just (t1, "<=" : ts') -> case parseTerm ts' of
		Just (t2, ")" : ts'') -> Just (t1 :<= t2, ts'')
		_ -> Nothing
	_ -> Nothing

parseEq :: [String] -> Maybe (Exp String Bool, [String])
parseEq ts = case parseVar ts of
	Just (v1, "==" : ts') -> case parseVar ts' of
		Just (v2, ")" : ts'') -> Just (Var v1 :== Var v2, ts'')
		_ -> case parseTerm ts' of
			Just (t2, ")" : ts'') -> Just (Var v1 :== t2, ts'')
			_ -> case parseBool ts' of
				Just (b2, ")" : ts'') -> Just (Var v1 :== b2, ts'')
				_ -> Nothing
	_ -> case parseTerm ts of
		Just (t1, "==" : ts') -> case parseTerm ts' of
			Just (t2, ")" : ts'') -> Just (t1 :== t2, ts'')
			_ -> Nothing
		_ -> case parseBool ts of
			Just (b1, "==" : ts') -> case parseBool ts' of
				Just (b2, ")" : ts'') -> Just (b1 :== b2, ts'')
				_ -> Nothing
			_ -> Nothing

parseVar :: [String] -> Maybe (String, [String])
parseVar (t : ts)
	| all isLower t = Just (t, ts)
parseVar _ = Nothing

parseTerm :: [String] -> Maybe (Exp String Term, [String])
parseTerm (t : ts)
	| all isLower t = Just (Var t, ts)
	| all isDigit t = Just (Const $ read t, ts)
parseTerm ("(" : ts) = case parseTerm ts of
	Just (t1, "+" : ts') ->  case parseTerm ts' of
		Just (t2, ")" : ts'') -> Just (t1 :+ t2, ts'')
		_ -> Nothing
	Just (t1, "-" : ts') -> case parseTerm ts' of
		Just (t2, ")" : ts'') -> Just (t1 :- t2, ts'')
		_ -> Nothing
	_ -> Nothing
parseTerm _ = Nothing

tokens :: String -> [String]
tokens = unfoldr lex'

lex' :: String -> Maybe (String, String)
lex' "" = Nothing
lex' s = listToMaybe $ lex s
