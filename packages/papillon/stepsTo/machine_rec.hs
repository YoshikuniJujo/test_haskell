import Control.Monad
import Data.Char

type Parse v = String -> Maybe (v, String)

run :: Parse Int
run s@(c : cs) | isDigit c = msum [ do
	(n, '+' : cs') <- run cs
	return (n + fromDigit c, cs'), do
	(n, '-' : cs') <- run cs
	return (n - fromDigit c, cs'),
	return (0, s) ]
run s = return (0, s)

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
