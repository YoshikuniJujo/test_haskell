{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.Middle where

import Foreign.Marshal.Array
import Control.Arrow
import Control.Monad.Cont
import Data.Word

import Vulkan.Enum

import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.Buffer.Middle as Buffer
import qualified Vulkan.Buffer.Core as Buffer.C
import qualified Vulkan.Device as Device
import qualified Vulkan.Command.Core as C

bindVertexBuffers ::
	CommandBuffer.C vs -> Word32 -> [(Buffer.B, Device.Size)] -> IO ()
bindVertexBuffers (CommandBuffer.C c)
	fb ((length &&& unzip) -> (bc, (bs, os))) = ($ pure) $ runContT do
	pb <- ContT $ allocaArray bc
	lift . pokeArray pb $ (\(Buffer.B b) -> b) <$> bs
	po <- ContT $ allocaArray bc
	lift . pokeArray po $ (\(Device.Size sz) -> sz) <$> os
	lift $ C.bindVertexBuffers c fb (fromIntegral bc) pb po

copyBuffer ::
	CommandBuffer.C vs -> Buffer.B -> Buffer.B -> [Buffer.C.Copy] -> IO ()
copyBuffer (CommandBuffer.C c) (Buffer.B s) (Buffer.B d)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift do	pokeArray prs rs
		C.copyBuffer c s d (fromIntegral rc) prs

bindIndexBuffer ::
	CommandBuffer.C vs -> Buffer.B -> Device.Size -> IndexType -> IO ()
bindIndexBuffer
	(CommandBuffer.C cb) (Buffer.B ib) (Device.Size sz) (IndexType it) =
	C.bindIndexBuffer cb ib sz it
