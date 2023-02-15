{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sample.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable.PeekPoke
import Data.Bits
import Data.Word

import Gpu.Vulkan.Sample.Enum

#include <vulkan/vulkan.h>

data CountAndMask = CountAndMask CountFlagBits (Maybe Mask) deriving Show

newtype Mask = Mask Integer deriving Show

countAndMaskToCore ::
	CountAndMask -> ((CountFlagBits, Ptr #{type VkSampleMask}) -> IO a) -> IO a
countAndMaskToCore foo f = case foo of
	CountAndMask cfb Nothing -> f (cfb, NullPtr)
	CountAndMask cfb@(CountFlagBits c) (Just (Mask m)) ->
		allocaArray ln \pm ->
		pokeArray pm (integerToWord32s ln m) >>
		f (cfb, pm)
		where ln = fromIntegral $ (c - 1) `div` 32 + 1

integerToWord32s :: Int -> Integer -> [Word32]
integerToWord32s ln _ | ln < 1 = []
integerToWord32s ln n =
	fromInteger (n .&. 0xffffffff) :
	integerToWord32s (ln - 1) (n `shiftR` 32)
