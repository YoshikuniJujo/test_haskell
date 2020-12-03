{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tribial.Greeting where

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
pMessage d = do
	(g, d') <- greeting d
	(n, d'') <- name d'
	pure ((g, n), d'')

pGreeting :: Derivs -> Maybe (Token, Derivs)
pGreeting d = do
	(t, d') <- token d
	case t of
		Hello -> pure (t, d')
		GoodBye -> pure (t, d')
		_ -> Nothing

pName :: Derivs -> Maybe (Token, Derivs)
pName d = do
	(t, d') <- token d
	case t of
		World -> pure (t, d')
		Yoshikuni -> pure (t, d')
		_ -> Nothing
