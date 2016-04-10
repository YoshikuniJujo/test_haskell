import Data.Tree

sample :: Tree Int
sample = Node 888 [Node 987 [], Node 2 [Node 351 [], Node 592 []]]

dupT :: a -> Tree a
dupT x = Node x [Node x []]

myJoin :: Tree (Tree a) -> Tree a
myJoin (Node (Node x ts) tts) = Node x $ ts ++ map myJoin tts
