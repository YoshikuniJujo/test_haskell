{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (go) where

import Control.Arrow
import Control.Monad.MaybeState
import Data.Char

go :: String -> Maybe Tree
go src = case parse [Nonterminal S] $ lexer src of
	(_, False) -> Nothing
	(rts, True) -> case mkTree `runMaybeState` rts of
		Just (r, []) -> Just r; _ -> Nothing

data Nonterminal = S | F
data Symbol = Nonterminal Nonterminal | T TokenIs
type TokenIs = Token -> Bool
data Token = LParen | RParen | Plus | Num Int deriving (Show, Eq)

isNum :: TokenIs; isNum = \case Num _ -> True; _ -> False

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

data RuleName = RuleOne | RuleTwo | RuleThree deriving (Show, Eq)
data RuleOrToken = Rule RuleName | Token Token deriving (Show, Eq)

parse :: [Symbol] -> [Token] -> ([RuleOrToken], Bool)
parse (Nonterminal S : st) ta@(LParen : _) =
	(Rule RuleTwo :) `first` parse (T (== LParen) : Nonterminal S : T (== Plus) : Nonterminal F : T (== RParen) : st) ta
parse (Nonterminal S : st) ta@(Num _ : _) = (Rule RuleOne :) `first` parse (Nonterminal F : st) ta
parse (Nonterminal F : st) ta@(Num _ : _) = (Rule RuleThree :) `first` parse (T isNum : st) ta
parse (T t0 : st) (t : ts) | t0 t = (Token t :) `first` parse st ts
parse [] [] = ([], True)
parse _ _ = ([], False)

maybeTokenNum :: RuleOrToken -> Maybe Int
maybeTokenNum = \case Token (Num n) -> Just n; _ -> Nothing

mkTree :: MaybeState [RuleOrToken] Tree
mkTree = getHead >>= \case
	Rule RuleOne -> mkTree
	Rule RuleTwo -> Pls
		<$> (matchHead (Token LParen) >> mkTree)
		<*> (matchHead (Token Plus) >>
			mkTree <* matchHead (Token RParen))
	Rule RuleThree -> N <$> maybeHead maybeTokenNum
	_ -> fail ""
