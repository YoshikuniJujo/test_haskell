{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree.Bits where

import Control.Arrow
import Data.Bits
import Data.Bool
import Data.ByteString.FingerTree.Internal

-- see ~/projects/tools-yj/src/Data/ByteString/ToolsYj.hs

fromBits :: Bits b => b -> ByteString
fromBits = unfoldr \b ->
	bool Nothing (Just (bitsToBits 8 b, b `shiftR` 8)) (b /= zeroBits)

fromBits' :: FiniteBits b => b -> ByteString
fromBits' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = bitsToBits 8 b `cons` go (n - 1) (b `shiftR` 8)

fromBitsBE :: Bits b => b -> ByteString
fromBitsBE b
	| b == zeroBits = ""
	| otherwise = fromBitsBE (b `shiftR` 8) `snoc` bitsToBits 8 b

fromBitsBE' :: FiniteBits b => b -> ByteString
fromBitsBE' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = go (n - 1) (b `shiftR` 8) `snoc` bitsToBits 8 b

bitsToBits :: (Bits a, Bits b) => Int -> a -> b
bitsToBits n b = foldl setBit zeroBits
	. map fst . filter snd $ map (id &&& testBit b) [0 .. n - 1]
