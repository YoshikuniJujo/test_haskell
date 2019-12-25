{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tips where

import Control.Monad
import Control.Concurrent
import Data.Bool
import System.Random

forkForever :: IO a -> IO ThreadId
forkForever = forkIO . forever

loopIf :: Monad m => m Bool -> m ()
loopIf act = bool (return ()) (loopIf act) =<< act

randomCycle :: StdGen -> [a] -> [a]
randomCycle gen lst = rc gen lst lst
	where
	rc g xs0 [] = rc g xs0 xs0
	rc g xs0 xs = let ((x, xs'), g') = randomPop g xs in x : rc g' xs0 xs'

randomPop :: StdGen -> [a] -> ((a, [a]), StdGen)
randomPop _ [] = error "bad"
randomPop g xs = ((xs !! i, take i xs ++ drop (i + 1) xs), g')
	where (i, g') = randomR (0, length xs - 1) g
