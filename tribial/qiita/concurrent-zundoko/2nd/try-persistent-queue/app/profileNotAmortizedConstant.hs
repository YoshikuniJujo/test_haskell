{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (head)

import ProfileQueue
import BatchedQueue

main :: IO ()
main = do
	{-# SCC "2000_ELEM_QUEUE" #-} trial 2000
	{-# SCC "4000_ELEM_QUEUE" #-} trial 4000

trial :: Int -> IO ()
trial n = print
	$ sum <$> sequence (timesEvaluate n head $ queueN @BatchedQueue n)

timesEvaluate :: Int -> (a -> b) -> a -> [b]
timesEvaluate n _ _ | n < 1 = []
timesEvaluate n f x = f x : timesEvaluate (n - 1) f x
