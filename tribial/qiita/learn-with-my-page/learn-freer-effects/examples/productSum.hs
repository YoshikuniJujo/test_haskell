data Product a b = a :*: b deriving Show
data Sum a b = L a | R b deriving Show

x :: Sum (Product Char Bool) (Product String Integer)
x = L $ 'c' :*: True
