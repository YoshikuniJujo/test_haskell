{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.List where

import Foreign.Marshal.Array
import Foreign.Storable
import Control.Arrow
import Control.Monad.Cont
import Data.IORef
import Data.Word
import TypeLevel.List hiding (length)

import qualified Foreign.Storable.Generic

import Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList (MapSubType)

import Gpu.Vulkan.Enum

import qualified Gpu.Vulkan.Buffer.List.Middle as Buffer.List
import qualified Gpu.Vulkan.Buffer.Middle as Buffer.M
import qualified Gpu.Vulkan.CommandBuffer.Middle as CommandBuffer
import qualified Gpu.Vulkan.Command.Middle as M
import qualified Gpu.Vulkan.Command.Core as C
import qualified Gpu.Vulkan.Image.Middle as Image
import qualified Gpu.Vulkan.Image.Enum as Image

bindVertexBuffers :: forall vs vs' .
	InfixIndex vs' (MapSubType vs) =>
	CommandBuffer.C vs -> Buffer.List.BList vs' -> IO ()
bindVertexBuffers cb bs =
	M.bindVertexBuffers cb (fromIntegral fb) (Buffer.List.bListToMList bs)
	where fb = infixIndex @vs' @(MapSubType vs)

copyBuffer :: Storable (Foreign.Storable.Generic.Wrap v) =>
	CommandBuffer.C vs -> Buffer.List.B v -> Buffer.List.B v -> Buffer.List.Copy v -> IO ()
copyBuffer cb s d cp = M.copyBuffer
	cb (Buffer.List.bToMiddle s) (Buffer.List.bToMiddle d) [Buffer.List.copyToCore cp]

bindIndexBuffer :: CommandBuffer.C vs -> Buffer.List.B v -> IndexType -> IO ()
bindIndexBuffer cb ib tp = M.bindIndexBuffer cb (Buffer.List.bToMiddle ib) 0 tp

copyBufferToImage ::
	CommandBuffer.C vs -> Buffer.List.B Word8 -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.C _ cb)
	(Buffer.List.B _ sb) (Image.I rdi) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift do	(_, di) <- readIORef rdi
		C.copyBufferToImage cb sb di dil (fromIntegral rc) prs
