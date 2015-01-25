import Control.Monad
import Data.Char

type Parse v = String -> Maybe (v, String)

a :: Parse Int

a (d : cs) | isDigit d = msum [
	case a cs of
		Just (n, '+' : cs') -> Just (n + fromDigit d, cs')
		_ -> Nothing,
	case a cs of
		Just (n, '-' : cs') -> Just (n - fromDigit d, cs')
		_ -> Nothing ]
a s = Just (0, s)

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
