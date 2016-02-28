import Data.Tree

mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f (Just x) = Just $ f x
mmap _ _ = Nothing

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Node x sf) = Node (f x) $ map (tmap f) sf
