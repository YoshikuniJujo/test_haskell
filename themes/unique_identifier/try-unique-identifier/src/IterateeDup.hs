{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeDup where

import System.IO.Unsafe

import Iteratee

add123 :: It Int Int
add123 = get >>= pure . a123
	where
	a123 :: Int -> Int
	a123 n = unsafePerformIO do
		putStrLn $ show n ++ " + 123 = " ++ show n'
		pure n'
		where n' = n + 123

result :: Maybe (Int, Int)
result = (`apply` [3, 4, 5]) do
	par add123 add123 >>= \case
		(Done l, Done r) -> pure (l, r)
		_ -> error "bad"
