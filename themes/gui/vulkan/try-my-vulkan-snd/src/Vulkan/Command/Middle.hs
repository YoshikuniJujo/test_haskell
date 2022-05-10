{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.Middle where

import Foreign.Marshal.Array
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.Buffer.Middle as Buffer
import qualified Vulkan.Device as Device
import qualified Vulkan.Command.Core as C

bindVertexBuffers ::
	CommandBuffer.C -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers (CommandBuffer.C c)
	fb ((length &&& unzip) -> (bc, (bs, os))) = ($ pure) $ runContT do
	pb <- ContT $ allocaArray bc
	lift . pokeArray pb $ (\(Buffer.B b) -> b) <$> bs
	po <- ContT $ allocaArray bc
	lift . pokeArray po $ (\(Device.Size sz) -> sz) <$> os
	lift $ C.bindVertexBuffers c fb (fromIntegral bc) pb po
