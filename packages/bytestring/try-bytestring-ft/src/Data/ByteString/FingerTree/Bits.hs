{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree.Bits where

import Control.Arrow
import Data.Bits
import Data.Bool
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Internal
import Data.MonoTraversable

-- see ~/projects/tools-yj/src/Data/ByteString/ToolsYj.hs

-- * FROM/TO BITS

fromBits :: Bits b => b -> ByteString
fromBits = unfoldr \b ->
	bool Nothing (Just (bitsToBits 8 b, b `shiftR` 8)) (b /= zeroBits)

fromBits' :: FiniteBits b => b -> ByteString
fromBits' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = bitsToBits 8 b `cons` go (n - 1) (b `shiftR` 8)

toBits :: Bits b => ByteString -> b
toBits = ofoldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

toBits' :: forall b . FiniteBits b => ByteString -> Maybe b
toBits' bs = bool
	Nothing
	(Just $ ofoldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits bs)
	(8 * olength bs <= finiteBitSize @b undefined)

-- * FROM/TO BITS -- BIG ENDIEN

fromBitsBE :: Bits b => b -> ByteString
fromBitsBE b
	| b == zeroBits = ""
	| otherwise = fromBitsBE (b `shiftR` 8) `snoc` bitsToBits 8 b

fromBitsBE' :: FiniteBits b => b -> ByteString
fromBitsBE' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = go (n - 1) (b `shiftR` 8) `snoc` bitsToBits 8 b

toBitsBE :: Bits b => ByteString -> b
toBitsBE = BSF.foldl' (\s b -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

toBitsBE' :: FiniteBits b => b -> ByteString
toBitsBE' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = go (n - 1) (b `shiftR` 8) `snoc` bitsToBits 8 b

-- * BITS TO BITS

bitsToBits :: (Bits a, Bits b) => Int -> a -> b
bitsToBits n b = foldl setBit zeroBits
	. map fst . filter snd $ map (id &&& testBit b) [0 .. n - 1]
