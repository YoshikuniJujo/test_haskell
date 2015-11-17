import Data.Tree
import Data.List

sampleTree :: Tree Int
sampleTree = Node 8 [Node 4 [Node 10 [], Node 2 []], Node 5 []]

class Queue q where
	enqueue :: Tree Int -> q -> q
	dequeue :: q -> Maybe (Tree Int, q)

step :: Queue q => q -> Maybe (Int, q)
step q = case dequeue q of
	Just (Node v cs, q') -> Just (v, foldr enqueue q' cs)
	_ -> Nothing


bfs :: Tree Int -> [Int]
bfs t = unfoldr step $ enqueue t []
