{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (go) where

import Control.Arrow
import Data.Bool
import Data.Char

data Element = S | F | T Token deriving Show

data Token = One | Plus | LParen | RParen deriving (Show, Eq)

data Tree = Pls Tree Tree | N Int deriving Show

go :: String -> Maybe Tree
go src = case parse [S] $ lexer src of
	(_, False) -> Nothing
	(rts, True) -> case mkTree `runMaybeState` rts of
		Just (r, []) -> Just r
		_ -> Nothing

lexer :: String -> [Token]
lexer = \case
	[] -> []
	'1' : s -> One : lexer s
	'+' : s -> Plus : lexer s
	'(' : s -> LParen : lexer s
	')' : s -> RParen : lexer s
	c : s | isSpace c -> lexer s
	_ -> error "no such token"

data RuleOrToken = Rule Int | Token Token deriving (Show, Eq)

parse :: [Element] -> [Token] -> ([RuleOrToken], Bool)
parse (S : st) ta@(LParen : _) =
	(Rule 2 :) `first` parse (T LParen : S : T Plus : F : T RParen : st) ta
parse (S : st) ta@(One : _) = (Rule 1 :) `first` parse (F : st) ta
parse (F : st) ta@(One : _) = (Rule 3 :) `first` parse (T One : st) ta
parse (T t0 : st) (t : ts) | t == t0 = (Token t :) `first` parse st ts
parse [] [] = ([], True)
parse _ _ = ([], False)

newtype MaybeState s a = MaybeState { runMaybeState :: s -> Maybe (a, s) }

instance Functor (MaybeState s) where
	f `fmap` MaybeState ms = MaybeState \s -> case ms s of
		Nothing -> Nothing
		Just (x, s') -> Just (f x, s')

instance Applicative (MaybeState s) where
	pure x = MaybeState \s -> Just (x, s)
	MaybeState mf <*> MaybeState mx = MaybeState \s -> case mf s of
		Nothing -> Nothing
		Just (f, s') -> case mx s' of
			Nothing -> Nothing
			Just (x, s'') -> Just (f x, s'')

instance Monad (MaybeState s) where
	MaybeState mx >>= f = MaybeState \s -> case mx s of
		Nothing -> Nothing
		Just (x, s') -> f x `runMaybeState` s'

instance MonadFail (MaybeState s) where fail _ = MaybeState \_ -> Nothing

get :: MaybeState s s
get = MaybeState \s -> Just (s, s)

put :: s -> MaybeState s ()
put s = MaybeState \_ -> Just ((), s)

getHead :: MaybeState [a] a
getHead = get >>= \case [] -> fail ""; x : xs -> x <$ put xs

matchHead :: Eq a => a -> MaybeState [a] ()
matchHead x0 = getHead >>= bool (fail "") (pure ()) . (== x0)

mkTree :: MaybeState [RuleOrToken] Tree
mkTree = do
	rt <- getHead
	case rt of
		Rule 1 -> mkTree
		Rule 2 -> do
			matchHead (Token LParen)
			t1 <- mkTree
			matchHead (Token Plus)
			t2 <- mkTree
			matchHead (Token RParen)
			pure $ Pls t1 t2
		Rule 3 -> do
			matchHead (Token One)
			pure $ N 1
		_ -> fail ""
