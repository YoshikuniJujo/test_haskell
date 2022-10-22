{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.SizeAlignment

import qualified Gpu.Vulkan.Enum as Vk
import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt

import qualified Cglm
import qualified Foreign.Storable.Generic

data Vertex = Vertex {
	vertexPos :: Pos,
	vertexColor :: Cglm.Vec3,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

newtype Pos = Pos Cglm.Vec3
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Pos Vertex
instance SizeAlignmentListUntil Cglm.Vec3 Vertex
instance SizeAlignmentListUntil TexCoord Vertex

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec2 where
	formatOf = Vk.FormatR32g32Sfloat

instance Vk.Ppl.VertexInputSt.Formattable Cglm.Vec3 where
	formatOf = Vk.FormatR32g32b32Sfloat

instance Foreign.Storable.Generic.G Vertex where
