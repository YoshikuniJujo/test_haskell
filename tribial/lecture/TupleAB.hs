module TupleAB (cons, uncons, Pair, p1, p2, p3, p4, p5) where

data Pair = Pair Integer Integer deriving Show

p1, p2, p3, p4, p5 :: Pair
p1 = Pair 2 3
p2 = Pair 5 7
p3 = Pair 4 9
p4 = Pair 10 21
p5 = Pair 15 16

cons :: Pair -> Integer -> Integer -> Integer
cons (Pair a b) x y = a ^ x * b ^ y

unpower :: Integer -> Integer -> Integer
unpower n x
	| x `mod` n /= 0 = 0
	| otherwise = 1 + unpower n (x `div` n)

uncons :: Pair -> Integer -> (Integer, Integer)
uncons (Pair a b) xy = (unpower a xy, unpower b xy)
