newtype List a = List {
	unList :: (a -> List a -> Either a (List a)) -> Either a (List a) }

list1 :: List Int
list1 = List $ \f -> f 1 list1

h :: a -> b -> Either a b
h x _ = Left x

t :: a -> b -> Either a b
t _ y = Right y

hd :: List a -> a
hd l = let Left x = unList l h in x

tl :: List a -> List a
tl l = let Right xs = unList l t in xs

-- list1 = \f -> f 1 list1
-- hd x y = x
-- tl x y = y
--
-- head l = l hd
--
-- head list1
-- list1 hd
-- (\f -> f 1 list1) hd
-- hd 1 list1
-- 1
