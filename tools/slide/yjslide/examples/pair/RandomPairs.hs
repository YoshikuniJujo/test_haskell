module RandomPairs (getRandomPairs, Word32) where

import System.Random
import Control.Monad
import Data.Word

getRandomPairs :: IO [(Word32, Word32)]
getRandomPairs = replicateM (10 ^ 4) $ do
	w1 <- randomIO
	w2 <- randomIO
	return (w1, w2)
