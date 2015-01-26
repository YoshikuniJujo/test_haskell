import Control.Monad
import Data.Char

type Result v = Maybe (v, String)

run :: String -> Maybe Int
run src | Just (n, _) <- rule src = Just n
run _ = Nothing

char :: String -> Result Char
char (x : xs) = Just (x, xs)
char _ = Nothing

rule :: String -> Result Int
rule s0 = msum [ do
	(c, s) <- char s0
	guard $ isDigit c
	(n, s') <- rule s
	('+', s'') <- char s'
	return (n + fromDigit c, s''), do
	(c, s) <- char s0
	guard $ isDigit c
	(n, s') <- rule s
	('-', s'') <- char s'
	return (n - fromDigit c, s''),
	return (0, s0) ]

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
