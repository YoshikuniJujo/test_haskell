data Token = Hello | GoodBye | World | Yoshikuni deriving Show

pMessage :: [Token] -> Maybe ((Token, Token), [Token])
pMessage ts = do
	(g, ts') <- pGreeting ts
	(n, ts'') <- pName ts'
	return ((g, n), ts'')

pGreeting :: [Token] -> Maybe (Token, [Token])
pGreeting (Hello : ts) = Just (Hello, ts)
pGreeting (GoodBye : ts) = Just (GoodBye, ts)
pGreeting _ = Nothing

pName :: [Token] -> Maybe (Token, [Token])
pName (World : ts) = Just (World, ts)
pName (Yoshikuni : ts) = Just (Yoshikuni, ts)
pName _ = Nothing
