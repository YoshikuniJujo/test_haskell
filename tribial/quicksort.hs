import Control.Monad
import Control.Monad.ST
import Data.Bool
import Data.List
import Data.Array.MArray
import Data.Array.ST

quicksort :: Ord a => [a] -> [a]
quicksort lst = runST
	$ (>>) <$> quicksortSTArray <*> getElems
		=<< newListArray (0, genericLength lst - 1) lst

quicksortSTArray :: (Ix i, Num i, Ord x) => STArray s i x -> ST s ()
quicksortSTArray a = uncurry qs =<< getBounds a
	where
	qs i0 j0
		| i0 >= j0 = return ()
		| otherwise = do
			x0 <- ra a i0
			k <- doWhile (i0 + 1, j0) $ \(i, j) -> do
				eij <- next x0 i0 j0
				case eij of
					Right (i, j) ->
						Right (i + 1, j - 1)
							<$ flipArray a i j
					Left j -> return $ Left j
			flipArray a i0 k >> qs i0 (k - 1) >> qs (k + 1) j0
	next x0 i0 j0 = doWhile (i0, j0) $ \(i, j) -> do
		[xi, xj] <- mapM (ra a) [i, j]
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

doWhile :: Monad m => a -> (a -> m (Either b a)) -> m b
doWhile x0 act = either return (`doWhile` act) =<< act x0

flipArray :: Ix i => STArray s i x -> i -> i -> ST s ()
flipArray a i j = zipWithM_ (writeArray a) [j, i] =<< mapM (ra a) [i, j]

ra :: Ix i => STArray s i x -> i -> ST s x
ra = readArray
