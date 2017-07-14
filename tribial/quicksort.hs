import Control.Monad (zipWithM_)
import Control.Monad.ST (ST, runST)
import Data.Bool (bool)
import Data.List (genericLength)
import Data.Array.MArray (
	getBounds, readArray, writeArray, newListArray, getElems)
import Data.Array.ST (STArray, Ix)

quicksort :: Ord a => [a] -> [a]
quicksort lst = runST
	$ (>>) <$> quicksortSTArray <*> getElems
		=<< newListArray (0, genericLength lst - 1) lst

quicksortSTArray :: (Ix i, Num i, Ord x) => STArray s i x -> ST s ()
quicksortSTArray a = (getBounds a >>=) . flip devideAndConquer $ \(i0, j0) ->
	flip (bool $ return Nothing) (i0 < j0) $ do
		x0 <- ra i0
		k <- doWhile (i0 + 1, j0) $ \(i, j) -> do
			eij <- next x0 i0 j0
			case eij of
				Right (i, j) -> Right (i + 1, j - 1) <$ fa i j
				Left i -> return $ Left i
		fa i0 k
		return $ Just ((i0, k - 1), (k + 1, j0))
	where
	next x0 i0 j0 = doWhile (i0, j0) $ \(i, j) -> do
		[xi, xj] <- mapM ra [i, j]
		return $ case (xi <= x0, x0 <= xj) of
			(True, True)
				| i + 1 < j -> Right (i + 1, j - 1)
				| otherwise -> Left $ Left i
			(True, False)
				| i + 1 < j -> Right (i + 1, j)
				| otherwise -> Left $ Left j
			(False, True)
				| i + 1 < j -> Right (i, j - 1)
				| otherwise -> Left . Left $ i - 1
			(False, False) -> Left $ Right (i, j)
	fa i j = zipWithM_ (writeArray a) [j, i] =<< mapM ra [i, j]
	ra = readArray a

doWhile :: Monad m => a -> (a -> m (Either b a)) -> m b
doWhile x0 act = either return (`doWhile` act) =<< act x0

devideAndConquer :: Monad m => a -> (a -> m (Maybe (a, a))) -> m ()
devideAndConquer x0 act = (act x0 >>=) . maybe (return ()) $ \(x1, x2) ->
	mapM_ (`devideAndConquer` act) [x1, x2]
