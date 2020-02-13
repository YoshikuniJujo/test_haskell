{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreerIterateeWithTag where

import System.IO.Unsafe

import FreerWithTag

data It i a where Get :: It i i

type Iteratee s i = Freer s (It i)

get :: Iteratee s i i
get = Get >>>= pure

sample :: Iteratee s Int Int
sample = do
	x <- get
	y <- get
	z <- unsafePerformIO $ putStrLn "foobarbaz" >> pure get
	pure $ unsafePerformIO $ putStrLn "barbarba" >> pure (x + y + z)

apply :: Iteratee s i a -> [i] -> Maybe a
Pure x `apply` _ = Just x
(Get :>>= _) `apply` [] = Nothing
(Get :>>= fs) `apply` (i : is) = (fs `qApp` i) `apply` is

par :: Iteratee s i a -> Iteratee s i b -> Iteratee s i (Iteratee s i a, Iteratee s i b)
par l r	| Get :>>= f <- l, Get :>>= g <- r = get >>= \x -> par (f `qApp` x) (g `qApp` x)
	| otherwise = pure (l, r)

par' :: Iteratee s i a -> Iteratee s i a -> Iteratee s i (Iteratee s i a, Iteratee s i a)
par' l r
	| Get :>>= f <- l, Get :>>= g <- r = get >>= \x -> uncurry par' $ qAppWith f g x
	| otherwise = pure (l, r)

sample2 :: Count s (Iteratee s Int (Int, Int))
sample2 = do
	s <- addTag sample
	pure do	(s1, s2) <- par' s (do r <- s; pure $ r + 15)
		(,) <$> s1 <*> s2
