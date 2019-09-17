{-# LANGUAGE TypeOperators, NoStarIsType #-}

data a * b = a :*: b deriving Show
data a + b = L a | R b deriving Show

x :: (Char * Bool) + (String * Integer)
x = L $ 'c' :*: True
