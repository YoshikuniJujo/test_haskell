{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseTagged where

import Tagged

add123 :: Int -> IO Int
add123 n = do
	putStrLn $  show n ++ " + 123 = " ++ show n'
	pure n'
	where
	n' = n + 123

sample :: Count s (Tagged s IO Int (Int, Int))
sample = do
	f <- mkTagged add123
	parpar f f

sample2 :: Count s (FTCQueue (Tagged s IO) Int Int)
sample2 = tsingleton <$> mkTagged add123
