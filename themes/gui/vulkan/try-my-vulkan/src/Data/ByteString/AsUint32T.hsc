{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.AsUint32T where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Data.Word
import Data.ByteString.Internal

#include <vulkan/vulkan.h>

useAsUint32TLen ::
	ByteString -> ((Ptr #{type uint32_t}, #{type size_t}) -> IO a) -> IO a
useAsUint32TLen (PS fp o l) action =
	allocaBytesAligned l #{alignment uint32_t} \buf ->
		withForeignPtr fp \p -> do
			memcpy buf (p `plusPtr` o) (fromIntegral l)
			action (castPtr buf, fromIntegral l)
