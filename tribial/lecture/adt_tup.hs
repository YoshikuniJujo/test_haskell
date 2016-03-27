tup x y f = f x y
ft x y = x
sd x y = y

ftt t = t ft

-- ftt (tup 3 10)
-- (tup 3 10) ft
-- tup 3 10 ft
-- ft 3 10
-- 3

data Tup = Tup Int Int deriving Show

ff, ss :: Tup -> Int
ff (Tup x _) = x
ss (Tup _ y) = y

-- Tup 10 15
-- \f -> f 10 15

data List = List Int List deriving Show

-- list = List 1 list
-- list = \f -> f 1 list
--
-- List 1 undefined
-- \f -> f 1 undefined
--


