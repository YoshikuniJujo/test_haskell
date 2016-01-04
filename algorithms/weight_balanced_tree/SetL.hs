module SetL (Set, empty, member, insert, delete) where

type Set a = [a]

empty :: Set a
empty = []

insert :: a -> Set a -> Set a
insert = (:)

delete :: Eq a => a -> Set a -> Set a
delete = filter . (/=)

member :: Eq a => a -> Set a -> Bool
member = elem
