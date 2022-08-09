{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.IORef
import Data.Bits
import Data.Bool
import Data.Maybe

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
