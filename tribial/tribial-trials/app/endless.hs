main :: IO ()
main = print endless

x :: Int
x = x

data BinTree a = BinTree a (BinTree a) (BinTree a) deriving Show

-- fromList :: [BinTree a] -> [a] -> BinTree a
-- fromList 

endless :: BinTree Int
endless = BinTree 1 endless endless
