{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BucketSort where

import Data.Array.MArray

bucket :: forall arr m a .
	(MArray arr Int m, Ix a, Enum a) => (a, a) -> [a] -> m [a]
bucket bs@(mn, mx) xs = do
	a :: arr a Int <- newArray bs 0
	mapM_ (flip (modifyArray a) (+ 1)) xs
	concat <$> mapM (result a) [mn .. mx]

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = writeArray a i . f =<< readArray a i

result :: (MArray arr Int m, Ix a) => arr a Int -> a -> m [a]
result arr x = (`replicate` x) <$> readArray arr x
