{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseUniqueMonadIO where

import System.IO.Unsafe

import UniqueMonad

add123 :: Int -> IO Int
add123 n = do
	putStrLn $ show n ++ " + 123 = " ++ show n'
	pure n'
	where
	n' = n + 123

mkAdd123 :: Int -> IO Int
mkAdd123 n = unsafePerformIO do
	putStrLn "make monad"
	pure $ add123 n

sample :: CountT s IO (Int, Int)
sample = do
	f <- mkMonad mkAdd123
	ff <- parMonad f f
	apply ff 321

sample2 :: CountT s IO (Int, Int)
sample2 = do
	f <- mkMonad mkAdd123
	g <- mkMonad mkAdd123
	fg <- parMonad f g
	apply fg 321
