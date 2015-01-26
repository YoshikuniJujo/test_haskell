import Control.Monad
import Data.Char

type Result v = Maybe (v, Derivs)

data Derivs = Derivs {
	rule :: Result Int,
	char :: Result Char }

run :: String -> Maybe Int
run src	| Just (n, _) <- rule $ derivs src = Just n
run _ = Nothing

derivs :: String -> Derivs
derivs src = d
	where
	d = Derivs rl ch
	rl = pRule d
	ch = case src of
		c : cs -> Just (c, derivs cs)
		_ -> Nothing

pRule :: Derivs -> Result Int
pRule d0 = msum [ do
	(c, d) <- char d0
	guard $ isDigit c
	(n, d') <- rule d
	('+', d'') <- char d'
	return (n + fromDigit c, d''), do
	(c, d) <- char d0
	guard $ isDigit c
	(n, d') <- rule d
	('-', d'') <- char d'
	return (n - fromDigit c, d''),
	return (0, d0) ]

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

sample :: Int -> String
sample n = take n (concat $ repeat "0123456789") ++
	replicate n '-'
