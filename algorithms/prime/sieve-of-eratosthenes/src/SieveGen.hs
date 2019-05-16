{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module SieveGen where

import Control.Arrow (first)
import Control.Monad.Fail
import Data.Proxy
import Data.Foldable (for_)
import Data.Array.MArray (MArray, newArray, getBounds, readArray, writeArray)
import Data.Bool (bool)

sieveM :: forall a m . (MArray a Bool m, MonadFail m) => Proxy a -> Int -> m [Int]
sieveM _ n | n < 2 = error "sieveIo n: n should be larger than 1"
sieveM _ n = do
	a :: a Int Bool <- newArray (2, n) True
	(ps, l) <- sieve 2 a
	(ps ++) <$> nexts l a
	where
	sieve i _ | i ^ (2 :: Int) > n = return ([i], i)
	sieve i a = do
		remove i a
		Just i' <- next i a
		first (i :) <$> sieve i' a

next :: MArray a Bool m => Int -> a Int Bool -> m (Maybe Int)
next n a = do
	(_, mx) <- getBounds a
	if n' > mx
		then return Nothing
		else bool (next n' a) (return $ Just n') =<< readArray a n'
	where n' = n + 1

remove :: MArray a Bool m => Int -> a Int Bool -> m ()
remove n a =  do
	(_, mx) <- getBounds a
	for_ (takeWhile (<= mx) [2 * n, 3 * n ..]) $ flip (writeArray a) False

nexts :: MArray a Bool m => Int -> a Int Bool -> m [Int]
nexts n a = maybe (return []) (\n' -> (n' :) <$> nexts n' a) =<< next n a
