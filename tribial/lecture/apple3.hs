import Data.Either
import Data.List

data Apple = Leaf | Flower | Apple deriving Show

data Tree a = Branch (Tree a) (Tree a) | Atom a deriving Show

priceA :: Apple -> Int
priceA Leaf = 50
priceA Flower = 80
priceA Apple = 100

price1 :: Tree Apple -> Int
price1 (Atom a) = priceA a
price1 (Branch t1 t2) = 20 + price1 t1 + price1 t2

dfs, bfs :: Tree a -> [a]
dfs (Atom a) = [a]
dfs (Branch t1 t2) = dfs t1 ++ dfs t2
bfs = concat . bfsl . (: [])

bfsl :: [Tree a] -> [[a]]
bfsl = unfoldr $ \t -> if null t then Nothing else let
	(ats, brs) = partitionEithers $ map branch t in
	Just (ats, concat brs)

branch :: Tree a -> Either a [Tree a]
branch (Atom a) = Left a
branch (Branch t1 t2) = Right [t1, t2]
