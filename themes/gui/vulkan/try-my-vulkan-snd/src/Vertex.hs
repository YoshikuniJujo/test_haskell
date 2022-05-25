{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.SizeAlignment

import qualified Foreign.Storable.Generic
import qualified Cglm

import qualified Vulkan.Format.Enum as Vk.Format
import qualified Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt

data Vertex = Vertex {
	vertexPos :: Cglm.Vec3,
	vertexColor :: Color,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

type WVertex = Foreign.Storable.Generic.Wrap Vertex

newtype Color = Color Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Cglm.Vec2 Vertex
instance SizeAlignmentListUntil Cglm.Vec3 Vertex
instance SizeAlignmentListUntil Color Vertex
instance SizeAlignmentListUntil TexCoord Vertex

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec2 where
	formatOf = Vk.Format.R32g32Sfloat

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec3 where
	formatOf = Vk.Format.R32g32b32Sfloat

instance Foreign.Storable.Generic.G Vertex
