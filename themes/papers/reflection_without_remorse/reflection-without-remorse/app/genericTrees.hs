{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GenericTree

main :: IO ()
main = do
	print $ {-# SCC "LEFT" #-} monad1 >>= funF >>= funG		-- 23 + 7 = 30	29 + 9 = 38
	print $ {-# SCC "RIGHT" #-} monad1 >>= (\x -> funF x >>= funG)	-- 27		34
