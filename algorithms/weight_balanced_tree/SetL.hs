module SetL (Set, empty, member, insert, delete) where

type Set a = [a]

empty :: Set a
empty = []

member :: Eq a => a -> Set a -> Bool
member = elem

insert :: a -> Set a -> Set a
insert = (:)

delete :: Eq a => a -> Set a -> Set a
delete = filter . (/=)
