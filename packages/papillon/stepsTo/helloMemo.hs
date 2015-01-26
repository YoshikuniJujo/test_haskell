data Token = Hello | GoodBye | World | Yoshikuni
	deriving Show

data Derivs = Derivs {
	message :: Maybe ((Token, Token), Derivs),
	greeting :: Maybe (Token, Derivs),
	name :: Maybe (Token, Derivs),
	token :: Maybe (Token, Derivs) }

parse :: [Token] -> Derivs
parse ts = d
	where
	d = Derivs m g n t
	m = pMessage d
	g = pGreeting d
	n = pName d
	t = case ts of
		(t : ts') -> Just (t, parse ts')
		_ -> Nothing

pMessage :: Derivs -> Maybe ((Token, Token), Derivs)
pMessage d = do
	(g, d') <- greeting d
	(n, d'') <- name d'
	return ((g, n), d'')

pGreeting :: Derivs -> Maybe (Token, Derivs)
pGreeting d = do
	(t, d') <- token d
	case t of
		Hello -> return (t, d')
		GoodBye -> return (t, d')
		_ -> fail "not parsed"

pName :: Derivs -> Maybe (Token, Derivs)
pName d = do
	(t, d') <- token d
	case t of
		World -> return (t, d')
		Yoshikuni -> return (t, d')
		_ -> fail "not parsed"
