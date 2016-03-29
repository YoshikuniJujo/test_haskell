{-# LANGUAGE RankNTypes #-}

import Control.Monad.Fix

ten = let f = \x -> if x < 1 then 1 else x * f (x - 1) in f 10

ten' = (\f -> f 10) . fix $ \f x -> if x < 1 then 1 else x * f (x - 1)

some = let x = 8; y = x + 3 in x + y

some' = (\fx x y -> fix fx x y + fix fx y x)
	(\fx x y -> x (fx x y) (fx y x))
	(\x y -> 8)
	(\y x -> x + 3)

some'' = (\x y -> fix2 x y + fix2 y x)
	(\x y -> 8)
	(\y x -> x + 3)

bar = let x = 8; y = "hello" in replicate x y

bar' = (\fx x y -> replicate (fix fx x y) (fix fx y x))
	(\fx x y -> x (fx x y) (fx y x))
	(\x y -> 8)
	(\y x -> 123)

fix2 :: (a -> b -> a) -> (b -> a -> b) -> a
fix2 f g = f (fix2 f g) (fix2 g f)

fix2' = fix $ \f2 f g -> f (f2 f g) (f2 g f)

-- fix2' :: (a -> b -> a) -> (b -> a -> b) -> a
{-
fix2' = fix ((\f2 -> (\f g -> f (f2 f g) (f2 g f))) :: 
	(forall a b . (a -> b -> a) -> (b -> a -> b) -> a) -> (a -> b -> a) -> (b -> a -> b) -> a)
	-}

-- foo = (\x y -> fix2' x y + fix2' y x)
--	(\x y -> 8)
--	(\y x -> x + 3)

{-
fix2' :: (forall a b . (a -> b -> a) -> (b -> a -> b) -> a) ->
	(forall a b . (a -> b -> a) -> (b -> a -> b) -> a)
fix2' = \f2 -> (\f g -> f (f2 f g) (f2 g f))
-}


kaerematen :: Int
kaerematen = fix (\f n -> if n < 0 then 0 else n + f (n - 1)) 10

-- fix2 x y + fix2 y x
-- fix2 (\x y -> 8) (\y x -> x + 3) + fix2 (\y x -> x + 3) (\x y -> 8)
-- (\x y -> 8) (fix2 ...) (fix2 ...) + (\y x -> x + 3) (fix2 ...) (fix2 ...)
-- 8 + (\y x -> x + 3) (fix2 ...) (fix2 (\x y -> 8) (\y x -> x + 3))
-- 8 + (\y x -> x + 3) (fix2 ...) ((\x y -> 8) (fix2 ...) (fix2 ...))
-- 8 + (\y x -> x + 3) (fix2 ...) 8
-- 8 + (8 + 3)
-- 19

-- some' = (\x y -> x + y) (fix2 $ \x y -> 8) (fix2 $ \x y -> x + 3)

other = let x = 8; y = x + 5; z = y * 2 in x + y + z

other' = (\fx x y z -> fix fx x y z + fix fx y z x + fix fx z x y)
	(\fx a b c -> (a (fx a b c) (fx b c a) (fx c a b)))
	(\x y z -> 8)
	(\y z x -> x + 5)
	(\z x y -> y * 2)

other'' = (\x y z -> fix3 x y z + fix3 y z x + fix3 z x y)
	(\x y z -> 8)
	(\y z x -> x + 5)
	(\z x y -> y * 2)

fix3 :: (a -> b -> c -> a) -> (b -> c -> a -> b) -> (c -> a -> b -> c) -> a
fix3 f g h = f (fix3 f g h) (fix3 g h f) (fix3 h f g)

-- マクロfixNを作成して$(fixN 3)のようにできると良いかもしれない。

--
-- fixN f g h ... x y z = f (fixN f g h ... x y z) (fixN g h i ... y z f)
-- 	(fixN h i j ... z f g) ... (fixN z f g ... w x y)
--
-- let
-- 	v1 = exp1
-- 	v2 = exp2
-- 	.
-- 	.
-- 	.
-- 	vn = expn in
-- 	fun v1 v2 ... vn
--
-- (\v1 v2 ... vn -> fun (fixN v1 v2 ... v(n-1) vn) (fixN v2 v3 ... vn v1)
-- 	... (fixN vn v1 ... v(n-2) v(n-1)))
-- 	(\v1 v2 ... v(n-1) vn -> exp1)
-- 	(\v2 v3 ... vn v1 -> exp2)
-- 	.
-- 	.
-- 	.
-- 	(\vn v1 ... v(n-2) v(n-1) -> expn)
