{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

import Control.Monad.Fix

fun = let f n = case n of 0 -> 0; _ -> n + f (n - 1) in f 10

fun' = (\f -> fix f 10) $ \f n -> case n of 0 -> 0; _ -> n + f (n - 1)

inf = let x = x in x

inf' = (\x -> fix x 10) $ \x -> x

simple = let x = 5 in x ^ x

simple' = (\x -> fix x ^ fix x) $ \x -> 5

simple'' = (\x' -> (\x -> x ^ x) $ fix x') $ \x -> 5

fg = let
	f n = case n of 0 -> 0; _ -> 2 + g (n - 1)
	g n = case n of 0 -> 0; _ -> 3 + f (n - 1) in
	f 10

fg' = (\fx f -> fix fx f 10)
	(\fx f -> f (fx f))
	(\f n -> case n of 0 -> 0; _ -> 2 + f (n - 1))

fg'' = (\fx f g -> fix fx f g 10)
	(\fx f g -> f (fx f g) (fx g f))
	(\f g n -> case n of 0 -> 0; _ -> 2 + g (n - 1))
	(\g f n -> case n of 0 -> 0; _ -> 3 + f (n - 1))

fgh = let
	f n = case n of 0 -> 0; _ -> 2 + g (n - 1)
	g n = case n of 0 -> 0; _ -> 3 + h (n - 1)
	h n = case n of 0 -> 0; _ -> 7 + f (n - 1) in
	f 10

fgh' = (\fx f g h -> fix fx f g h 10)
	(\fx f g h -> f (fx f g h) (fx g h f) (fx h f g))
	(\f g h n -> case n of 0 -> 0; _ -> 2 + g (n - 1))
	(\g h f n -> case n of 0 -> 0; _ -> 3 + h (n - 1))
	(\h f g n -> case n of 0 -> 0; _ -> 7 + f (n - 1))

fix2 :: (a -> b -> a) -> (b -> a -> b) -> a
fix2 f g = f (fix2 f g) (fix2 g f)

fix3 :: (a -> b -> c -> a) -> (b -> c -> a -> b) -> (c -> a -> b -> c) -> a
fix3 f g h = f (fix3 f g h) (fix3 g h f) (fix3 h f g)

fix3' f g h = f (fix3' f g h) (fix3' g h f) (fix3' h f g)

fgx = (\f g h -> fix3 f g h 10)
	(\f g h n -> case n of 0 -> 0; _ -> 2 + g (n - 1))
	(\g h f n -> case n of 0 -> 0; _ -> 3)
	(\h f g -> 10)

type T a b c = (a -> b -> c -> a) -> (b -> c -> a -> b) -> (c -> a -> b -> c) -> a

{-
fgx' = (\fx f g h -> fix fx f g h 10)
	((\fx f g h -> f (fx f g h) (fx g h f) (fx h f g)) ::
--		(forall a b c . T a b c) -> a -> b -> c -> a)
		((forall a b c . T a b c) ->
			(a -> b -> c -> a) ->
			(b -> c -> a -> b) ->
			(c -> a -> b -> c) -> a)
		)
	(\f g h n -> case n of 0 -> 0; _ -> 2 + g (n - 1))
	(\g h f n -> case n of 0 -> 0; _ -> 3 + h (n - 1))
	(\h f g n -> case n of 0 -> 0; _ -> 7 + f (n - 1))
--	(\h f g -> 10 :: Int)
-}
