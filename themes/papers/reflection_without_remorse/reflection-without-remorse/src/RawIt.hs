{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RawIt where

import Prelude hiding ((>>=))

data It i a = Get (i -> It i a) | Done a

ret :: a -> It i a
ret = Done

infixr 1 >=>, >>=

(>=>) :: (a -> It i b) -> (b -> It i c) -> (a -> It i c)
(f >=> g) x = f x >>= g

(>>=) :: It i a -> (a -> It i b) -> It i b
Done r >>= f = f r
Get h >>= f = Get $ h >=> f

-- f x = Get (Done . (+ x))
--
-- ((f >=> f) >=> f) x
-- (f >=> f) x >>= f
-- (f x >>= f) >>= f
-- (Get (Done . (+ x)) >>= f) >>= f
-- Get (Done . (+ x) >=> f) >>= f
-- Get ((Done . (+ x) >=> f) >=> f)
--
-- (f >=> (f >=> f)) x
-- f x >>= (f >=> f)
-- Get (Done . (+ x)) >>= (f >=> f)
-- Get (Done . (+ x) >=> (f >=> f))

get :: It i i
get = Get ret

sumInputL, sumInputR :: Int -> Int -> It Int Int
sumInputL n = foldl (>=>) ret $ replicate (n - 1) f
	where
	f x = ($ ()) $ const get >=> ret . (+ x)

sumInputR n = foldr (>=>) ret $ replicate (n - 1) f
	where
	f x = ($ ()) $ const get >=> ret . (+ x)

feedAll :: It a b -> [a] -> Maybe b
feedAll (Done a) _ = Just a
feedAll _ [] = Nothing
feedAll (Get f) (h : t) = feedAll (f h) t

testSumInputL, testSumInputR :: Int -> Maybe Int
testSumInputL n = sumInputL n 1 `feedAll` [2 .. n]
testSumInputR n = sumInputR n 1 `feedAll` [2 .. n]
