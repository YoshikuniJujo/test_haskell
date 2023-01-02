{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Sample.Middle.Internal where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Bits
import Data.Word

import Gpu.Vulkan.Sample.Enum

#include <vulkan/vulkan.h>

data CountAndMask = CountAndMask CountFlagBits (Maybe Mask) deriving Show

newtype Mask = Mask Integer deriving Show

countAndMaskToCore ::
	CountAndMask -> ContT r IO (CountFlagBits, Ptr #{type VkSampleMask})
countAndMaskToCore = \case
	CountAndMask cfb Nothing -> pure (cfb, NullPtr)
	CountAndMask cfb@(CountFlagBits c) (Just (Mask m)) -> do
		pm <- ContT $ allocaArray ln
		lift . pokeArray pm $ integerToWord32s ln m
		pure (cfb, pm)
		where ln = fromIntegral $ (c - 1) `div` 32 + 1

integerToWord32s :: Int -> Integer -> [Word32]
integerToWord32s ln _ | ln < 1 = []
integerToWord32s ln n =
	fromInteger (n .&. 0xffffffff) :
	integerToWord32s (ln - 1) (n `shiftR` 32)
