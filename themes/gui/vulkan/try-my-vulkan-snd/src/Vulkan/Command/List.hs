{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.List where

import GHC.Generics
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Arrow
import Control.Monad.Cont
import Data.Word
import TypeLevel.List

import qualified Foreign.Storable.Generic

import Data.Generics.Flatten (Flatten)
import Vulkan.Pipeline.VertexInputState.BindingStrideList (Simplify, MapUnList, MapSubType)

import Vulkan.Enum

import qualified Vulkan.Buffer.List as Buffer
import qualified Vulkan.Buffer.List as Buffer.List
import qualified Vulkan.Buffer.Middle as Buffer.M
import qualified Vulkan.CommandBuffer.Middle as CommandBuffer
import qualified Vulkan.Command.Middle as M
import qualified Vulkan.Command.Core as C
import qualified Vulkan.Image.Middle as Image
import qualified Vulkan.Image.Enum as Image

bindVertexBuffers :: forall vs vs' .
	InfixIndex vs' (MapUnList (MapSubType (Flatten (Rep vs)))) =>
	CommandBuffer.C vs -> Buffer.BList vs' -> IO ()
bindVertexBuffers cb bs =
	M.bindVertexBuffers cb (fromIntegral fb) (Buffer.bListToMList bs)
	where fb = infixIndex @vs' @(Simplify vs)

copyBuffer :: Storable (Foreign.Storable.Generic.Wrap v) =>
	CommandBuffer.C vs -> Buffer.B v -> Buffer.B v -> Buffer.Copy v -> IO ()
copyBuffer cb s d cp = M.copyBuffer
	cb (Buffer.bToMiddle s) (Buffer.bToMiddle d) [Buffer.copyToCore cp]

bindIndexBuffer :: CommandBuffer.C vs -> Buffer.B v -> IndexType -> IO ()
bindIndexBuffer cb ib tp = M.bindIndexBuffer cb (Buffer.bToMiddle ib) 0 tp

copyBufferToImage ::
	CommandBuffer.C vs -> Buffer.List.B Word8 -> Image.I -> Image.Layout ->
	[Buffer.M.ImageCopy] -> IO ()
copyBufferToImage (CommandBuffer.C cb)
	(Buffer.List.B sb) (Image.I di) (Image.Layout dil)
	(length &&& id -> (rc, rs)) = ($ pure) $ runContT do
	prs <- ContT $ allocaArray rc
	lift . pokeArray prs $ Buffer.M.imageCopyToCore <$> rs
	lift $ C.copyBufferToImage cb sb di dil (fromIntegral rc) prs
