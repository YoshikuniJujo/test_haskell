{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Command.List where

import GHC.Generics
import TypeLevel.List

import Data.Generics.Flatten (Flatten)
import Vulkan.Pipeline.VertexInputState.BindingStrideList (Simplify, MapUnList, MapSubType)

import qualified Vulkan.Buffer.List as Buffer
import qualified Vulkan.CommandBuffer as CommandBuffer
import qualified Vulkan.Command.Middle as M

bindVertexBuffers :: forall vs vs' .
	InfixIndex vs' (MapUnList (MapSubType (Flatten (Rep vs)))) =>
	CommandBuffer.C vs -> Buffer.BList vs' -> IO ()
bindVertexBuffers cb bs =
	M.bindVertexBuffers cb (fromIntegral fb) (Buffer.bListToMList bs)
	where fb = infixIndex @vs' @(Simplify vs)
