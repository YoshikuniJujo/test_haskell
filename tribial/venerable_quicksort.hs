import Control.Monad.ST (ST, runST)
import Data.List (genericLength)
import Data.Array.MArray (
	getBounds, readArray, writeArray, newListArray, getElems)
import Data.Array.ST (STArray, Ix)

quicksort :: Ord a => [a] -> [a]
quicksort lst = runST
	$ (>>) <$> quicksortSTArray <*> getElems
		=<< newListArray (0, genericLength lst - 1) lst

quicksortSTArray :: (Ix i, Num i, Ord x) => STArray s i x -> ST s ()
quicksortSTArray = undefined
