{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.ST
import Data.Bool
import Data.List
import Data.Array.MArray
import Data.Array.ST

quicksort :: Ord a => [a] -> [a]
quicksort lst = runST $ do
	a <- newListArray (0, genericLength lst - 1) lst
	quicksortSTArray a
	getElems a

quicksortSTArray :: forall i s x . (Ix i, Num i, Ord x) =>
	STArray s i x -> ST s ()
quicksortSTArray a = do
	(s, e) <- getBounds a
	qs s e 
	where
	ra :: STArray s i x -> i -> ST s x
	ra = readArray
	qs i0 j0
		| i0 >= j0 = return ()
		| otherwise = do
			x0 <- ra a i0
			k <- doWhile (i0 + 1, j0) $ \(i, j) -> do
				mij <- next x0 i0 j0
				case mij of
					Right (i, j) -> do
						flipArray a i j
						return $ Right (i + 1, j - 1)
					Left j -> return $ Left j
			flipArray a i0 k
			qs i0 (k - 1)
			qs (k + 1) j0
	next x0 i j = do
		xi <- ra a i
		xj <- ra a j
		case (xi <= x0, x0 <= xj) of
			(True, True)
				| i >= j - 1 -> return $ Left i
				| otherwise -> next x0 (i + 1) (j - 1)
			(True, False)
				| i >= j - 1 -> return $ Left j
				| otherwise -> next x0 (i + 1) j
			(False, True)
				| i >= j - 1 -> return . Left $ i - 1
				| otherwise -> next x0 i (j - 1)
			(False, False) -> return $ Right (i, j)

doWhile :: Monad m => a -> (a -> m (Either b a)) -> m b
doWhile x0 act = do
	v <- act x0
	case v of
		Left r -> return r
		Right x -> doWhile x act

flipArray :: Ix i => STArray s i x -> i -> i -> ST s ()
flipArray a i j = do
	xi <- readArray a i
	xj <- readArray a j
	writeArray a i xj
	writeArray a j xi
