import Data.List
import GaleShapley

data Man = A | B | C | D deriving (Show, Eq, Ord)
data Woman = X | Y | Z | W deriving (Show, Eq, Ord)

manT :: Table Man Woman
manT = [
	(A, [Y, X, Z, W]), (B, [X, W, Y, Z]),
	(C, [Y, X, Z, W]), (D, [W, Z, Y, X]) ]

womanT :: Table Woman Man
womanT = [
	(X, [C, D, B, A]), (Y, [D, B, A, C]),
	(Z, [A, C, B, D]), (W, [C, B, A, D]) ]

main :: IO ()
main = print . sort $ galeShapley manT womanT
