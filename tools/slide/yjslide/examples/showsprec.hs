infixr 5 :^:

data Tree a = Leaf a | Tree a :^: Tree a deriving Show
