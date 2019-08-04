{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module AccumArray where

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST

import MArrayTools

accumArray' :: forall a i e . Ix i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray' op e0 bs ies = runST $ do
	a <- newArray bs e0 :: ST s (STArray s i e)
	(\(i, x) -> modifyArray a i (`op` x)) `mapM_` ies
	freeze a

bucketSortAA :: Ix i => (i, i) -> [i] -> [i]
bucketSortAA bs is = concat $ (\i -> replicate (a ! i) i) <$> range bs
	where a = accumArray' (+) 0 bs (zip is $ repeat 1)
