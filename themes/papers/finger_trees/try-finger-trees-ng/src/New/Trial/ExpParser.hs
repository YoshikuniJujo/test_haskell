{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Trial.ExpParser where

import Data.Char

import New.Expression

parse :: String -> Maybe (Exp Char Bool, String)
parse ('F': cs) = Just (Bool False, cs)
parse ('T': cs) = Just (Bool True, cs)

parseTerm :: String -> Maybe (Exp Char Term, String)
parseTerm (c : cs)
	| isDigit c = Just (Const $ read [c], cs)
	| isLower c = Just (Var c, cs)
	{-
parseTerm ('(' : cs) = let
	(t, cs') = parseTerm 
	-}
parseTerm _ = Nothing
