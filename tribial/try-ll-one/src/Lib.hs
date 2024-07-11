{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (goW) where

import Data.Char
import LLOne

goW :: String -> Maybe Tree
goW = go rules tokenNumToTree S lexer

rules :: Rules Nonterminal Token Tree
rules = [
	(\nt tkn -> nt == S && isNum tkn, ([Nonterminal F], head)),
	(\nt tkn -> nt == S && tkn == LParen, (
		[	Terminal (== LParen), Nonterminal S,
			Terminal (== Plus), Nonterminal F,
			Terminal (== RParen) ],
		\[_, t1, _, t2, _] -> Pls t1 t2 )),
	(\nt tkn -> nt == F && isNum tkn, ([Terminal isNum], head)) ]

data Nonterminal = S | F deriving Eq
data Token = LParen | RParen | Plus | Num Int deriving (Show, Eq)
data Tree = Pls Tree Tree | N Int deriving Show

lexer :: String -> [Token]
lexer = \case
	[] -> []
	'+' : s -> Plus : lexer s
	'(' : s -> LParen : lexer s
	')' : s -> RParen : lexer s
	c : s	| isDigit c -> Num (read [c]) : lexer s
		| isSpace c -> lexer s
	_ -> error "no such token"

isNum :: TokenIs Token; isNum = \case Num _ -> True; _ -> False

tokenNumToTree :: Token -> Tree
tokenNumToTree = \case Num n -> N n; _ -> error "bad"
