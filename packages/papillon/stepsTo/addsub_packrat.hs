import Control.Monad
import Data.Char

type Result v = Maybe (v, Derivs)

data Derivs = Derivs {
	rule :: Result Int,
	char :: Result Char }

parse :: String -> Derivs
parse src = d
	where
	d = Derivs r c
	r = pRule d
	c = case src of
		(c : cs) -> Just (c, parse cs)
		_ -> Nothing

pRule :: Derivs -> Result Int
pRule d = msum [ do
	(c, d') <- char d
	guard (isDigit c)
	(n, d'') <- rule d'
	('+', d''') <- char d''
	return (n + fromDigit c, d'''), do
	(c, d') <- char d
	guard (isDigit c)
	(n, d'') <- rule d'
	('-', d''') <- char d''
	return (n - fromDigit c, d'''), do
	return (0, d) ]

fromDigit :: Char -> Int
fromDigit '0' = 0
fromDigit '1' = 1
fromDigit '2' = 2
fromDigit '3' = 3
fromDigit '4' = 4
fromDigit '5' = 5
fromDigit '6' = 6
fromDigit '7' = 7
fromDigit '8' = 8
fromDigit '9' = 9
fromDigit c = error $ show c ++ " is not digit."
