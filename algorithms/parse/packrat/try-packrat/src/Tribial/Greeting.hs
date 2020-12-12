{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tribial.Greeting where

import Control.Applicative
import Data.Parse

data Token = Hello | GoodBye | World | Yoshikuni deriving Show

data Derivs = Derivs {
	message :: Maybe ((Token, Token), Derivs),
	greeting :: Maybe (Token, Derivs),
	name :: Maybe (Token, Derivs),
	token :: Maybe (Token, Derivs) }

parseIt :: [Token] -> Maybe (Token, Token)
parseIt ts = fst <$> message (derivs ts)

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
pMessage = unparse $ (,) <$> parse pGreeting <*> parse pName

pGreeting :: Derivs -> Maybe (Token, Derivs)
pGreeting = unparse do
	t <- parse token
	case t of Hello -> pure t; GoodBye -> pure t; _ -> empty

pName :: Derivs -> Maybe (Token, Derivs)
pName = unparse do
	t <- parse token
	case t of World -> pure t; Yoshikuni -> pure t; _ -> empty
