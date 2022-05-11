{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.List where

import GHC.Generics
import Foreign.Storable
import TypeLevel.List

import qualified Foreign.Storable.Generic

import Data.Generics.Flatten (Flatten)
import Vulkan.Pipeline.VertexInputState.BindingStrideList (Simplify, MapUnList, MapSubType)

import Vulkan.Enum

import qualified Vulkan.Buffer.List as Buffer
import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.Command.Middle as M

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
