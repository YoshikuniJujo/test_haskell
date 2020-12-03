{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tribial.Greeting where

import Data.Parse

data Token = Hello | GoodBye | World | Yoshikuni deriving Show

data Derivs = Derivs {
	message :: Maybe ((Token, Token), Derivs),
	greeting :: Maybe (Token, Derivs),
	name :: Maybe (Token, Derivs),
	token :: Maybe (Token, Derivs) }

parse :: [Token] -> Maybe (Token, Token)
parse ts = fst <$> message (derivs ts)

derivs :: [Token] -> Derivs
derivs ts = d where
	d = Derivs m g n tk
	m = pMessage d
	g = pGreeting d
	n = pName d
	tk = case ts of
		(t : ts') -> Just (t, derivs ts')
		_ -> Nothing

pMessage :: Derivs -> Maybe ((Token, Token), Derivs)
Parse pMessage = (,) <$> Parse pGreeting <*> Parse pName

pGreeting :: Derivs -> Maybe (Token, Derivs)
Parse pGreeting = do
	t <- Parse token
	case t of Hello -> pure t; GoodBye -> pure t; _ -> fail "parse fail"

pName :: Derivs -> Maybe (Token, Derivs)
Parse pName = do
	t <- Parse token
	case t of World -> pure t; Yoshikuni -> pure t; _ -> fail "parse fail"
