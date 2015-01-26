data Token = Hello | GoodBye | World | Yoshikuni deriving Show

pMessage :: [Token] -> Maybe ((Token, Token), [Token])
pMessage ts = case pGreeting ts of
	Just (g, ts') -> case pName ts' of
		Just (n, ts'') -> Just ((g, n), ts'')
		_ -> Nothing
	_ -> Nothing

pGreeting :: [Token] -> Maybe (Token, [Token])
pGreeting (Hello : ts) = Just (Hello, ts)
pGreeting (GoodBye : ts) = Just (GoodBye, ts)
pGreeting _ = Nothing

pName :: [Token] -> Maybe (Token, [Token])
pName (World : ts) = Just (World, ts)
pName (Yoshikuni : ts) = Just (Yoshikuni, ts)
pName _ = Nothing
