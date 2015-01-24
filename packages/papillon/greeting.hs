data Token
	= Hello
	| GoodBye
	| World
	| Yoshikuni
	deriving Show

data Result v
	= Parsed v Derivs
	| NoParse

data Derivs = Derivs {
	dvMessage :: Result (Token, Token),
	dvGreeting :: Result Token,
	dvName :: Result Token,
	dvToken :: Result Token }

eval :: [Token] -> Maybe (Token, Token)
eval ts = case dvMessage (parse ts) of
	Parsed v _rem -> Just v
	_ -> Nothing

parse :: [Token] -> Derivs
parse ts = d where
	d = Derivs message greeting name token
	message = pMessage d
	greeting = pGreeting d
	name = pName d
	token = case ts of
		(t : ts') -> Parsed t (parse ts')
		_ -> NoParse

pMessage :: Derivs -> Result (Token, Token)
pMessage d = case dvGreeting d of
	Parsed g d' -> case dvName d' of
		Parsed n d'' -> Parsed (g, n) d''
		_ -> NoParse
	_ -> NoParse

pGreeting :: Derivs -> Result Token
pGreeting d = case dvToken d of
	Parsed Hello d' -> Parsed Hello d'
	Parsed GoodBye d' -> Parsed GoodBye d'
	_ -> NoParse

pName :: Derivs -> Result Token
pName d = case dvToken d of
	Parsed World d' -> Parsed World d'
	Parsed Yoshikuni d' -> Parsed Yoshikuni d'
	_ -> NoParse
