{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.SizeAlignment

import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt

import Gpu.Vulkan.Cglm qualified as Cglm
import qualified Foreign.Storable.Generic

data Vertex = Vertex {
	vertexPos :: Pos,
	vertexColor :: Color,
	vertexTexCoord :: TexCoord }
	deriving (Show, Eq, Ord, Generic)

newtype Pos = Pos Cglm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance Storable Vertex where
	sizeOf = Foreign.Storable.Generic.gSizeOf
	alignment = Foreign.Storable.Generic.gAlignment
	peek = Foreign.Storable.Generic.gPeek
	poke = Foreign.Storable.Generic.gPoke

instance SizeAlignmentList Vertex

instance SizeAlignmentListUntil Pos Vertex
instance SizeAlignmentListUntil Color Vertex
instance SizeAlignmentListUntil TexCoord Vertex

instance Foreign.Storable.Generic.G Vertex where

newtype Color = Color Cglm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)
