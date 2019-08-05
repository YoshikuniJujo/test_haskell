{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MArrayTools where

import Control.Arrow
import Data.Array.MArray

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray a i f = writeArray a i . f =<< readArray a i

scanMArray :: (MArray a e m, Ix i) => (s -> e -> s) -> a i e ->  s -> m ([s], s)
scanMArray op a s0 = do
	is <- range <$> getBounds a
	sma s0 is
	where
	sma s [] = return ([], s)
	sma s (i : is) = do
		e <- readArray a i
		first (s :) <$> sma (s `op` e) is
