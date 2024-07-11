{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (go) where

import Control.Arrow
import Data.Bool
import Data.Char

data Element = S | F | T (Token -> Bool)

data Token = Num Int | Plus | LParen | RParen deriving (Show, Eq)

isNum :: Token -> Bool
isNum = \case Num _ -> True; _ -> False

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
--	'1' : s -> One : lexer s
	'+' : s -> Plus : lexer s
	'(' : s -> LParen : lexer s
	')' : s -> RParen : lexer s
	c : s	| isDigit c -> Num (read [c]) : lexer s
		| isSpace c -> lexer s
	_ -> error "no such token"

data RuleOrToken = Rule Int | Token Token deriving (Show, Eq)

parse :: [Element] -> [Token] -> ([RuleOrToken], Bool)
parse (S : st) ta@(LParen : _) =
	(Rule 2 :) `first` parse (T (== LParen) : S : T (== Plus) : F : T (== RParen) : st) ta
parse (S : st) ta@(Num _ : _) = (Rule 1 :) `first` parse (F : st) ta
parse (F : st) ta@(Num _ : _) = (Rule 3 :) `first` parse (T isNum : st) ta
parse (T t0 : st) (t : ts) | t0 t = (Token t :) `first` parse st ts
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

checkHead :: (a -> Bool) -> MaybeState [a] a
checkHead p = getHead >>= \h ->
	bool (fail "") (pure h) (p h)

checkHead' :: (a -> Maybe b) -> MaybeState [a] b
checkHead' p =
	getHead >>= \h -> case p h of Nothing -> fail ""; Just r -> pure r

isTokenNum :: RuleOrToken -> Bool
isTokenNum = \case Token (Num _) -> True; _ -> False

isTokenNum' :: RuleOrToken -> Maybe Int
isTokenNum' = \case Token (Num n) -> Just n; _ -> Nothing

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
			n <- checkHead' isTokenNum'
			pure $ N n
		_ -> fail ""
