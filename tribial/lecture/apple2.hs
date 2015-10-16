import Data.Either
import Data.List

data Apple = Leaf | Flower | Apple | Branch Apple Apple
	deriving Show

price1 :: Apple -> Int
price1 Leaf = 50
price1 Flower = 80
price1 Apple = 100
price1 (Branch a1 a2) = 20 + price1 a1 + price1 a2

dfs, bfs :: Apple -> [Apple]
dfs (Branch a1 a2) = dfs a1 ++ dfs a2
dfs a = [a]
bfs = concat . bfsl . (: [])

branch :: Apple -> Either Apple [Apple]
branch (Branch a1 a2) = Right [a1, a2]
branch a = Left a

bfsl :: [Apple] -> [[Apple]]
bfsl = unfoldr $ \as -> if null as then Nothing else let
	(ats, brs) = partitionEithers $ map branch as in
	Just (ats, concat brs)
