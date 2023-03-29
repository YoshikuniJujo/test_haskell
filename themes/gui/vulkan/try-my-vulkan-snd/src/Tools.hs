{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.IORef
import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List

toBits :: FiniteBits a => a -> [a]
toBits x =
	catMaybes $ checkBit x . (bit 0 `shiftL`) <$> [0 .. finiteBitSize x - 1]

checkBit :: FiniteBits a => a -> a -> Maybe a
checkBit bs b = bool Nothing (Just b) $ bs .&. b /= zeroBits

clamp :: Ord a => a -> a -> a -> a
clamp x mn mx | x < mn = mn | x < mx = x | otherwise = mx

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x | p x = Just x | otherwise = Nothing

checkFlag :: IORef Bool -> IO Bool
checkFlag fg = readIORef fg >>= bool (pure False) (True <$ writeIORef fg False)

checkBits :: Bits bs => bs -> bs -> Bool
checkBits wnt = (== wnt) . (.&. wnt)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

findPlus :: (a -> Maybe b) -> [a] -> Maybe (a, b)
findPlus prd = \case
	[] -> Nothing; x : xs -> case prd x of
		Nothing -> findPlus prd xs; Just y -> Just (x, y)

findPlusM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe (a, b))
findPlusM prd = \case
	[] -> pure Nothing; x : xs -> prd x >>= \case
		Nothing -> findPlusM prd xs; Just y -> pure $ Just (x, y)

findBySnd :: (b -> Bool) -> [(a, b)] -> Maybe a
findBySnd p = (fst <$>) . find (p . snd)

data Inf a = a :- Inf a deriving Show

cycleI :: [a] -> Inf a
cycleI xs = go xs where go = \case [] -> cycleI xs; y : ys -> y :- go ys
