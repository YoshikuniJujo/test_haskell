{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Some (target') where

import Control.Monad.ST
import Data.Array.ST
import Data.Bool

lft, rgt :: Int -> Int
lft = (+ 1) . (* 2)
rgt = (+ 2) . (* 2)

target' :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s (Maybe Int)
target' a sz i x = a ! h >>= \case
	Nothing -> pure Nothing
	Just l -> next' x (h, l) . ((j ,) <$>) <$> a ! j
	where h = lft i; j = rgt i

(!) :: (MArray a e m, Ix i) => a i e -> i -> m (Maybe e)
a ! i = do
	(l, u) <- getBounds a
	bool (pure Nothing) (Just <$> readArray a i) (l <= i && i <= u)

next' :: Ord a => a -> (Int, a) -> Maybe (Int, a) -> Maybe Int
next' x (h, l) = \case
	Nothing -> bool Nothing (Just h) (x < l)
	Just (j, r)
		| l > r -> bool Nothing (Just h) (x < l)
		| otherwise -> bool Nothing (Just j) (x < r)
