{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex (WVertex, Vertex(..), Pos(..), Color(..), TexCoord(..)) where

import GHC.Generics
import Foreign.Storable
import Foreign.Storable.Generic qualified as GStorable

import Gpu.Vulkan.Pipeline.VertexInputState qualified as Vk.Ppl.VertexInputSt
import Gpu.Vulkan.Cglm qualified as Cglm

type WVertex = GStorable.W Vertex

data Vertex = Vertex {
	vertexPos :: Pos, vertexColor :: Color, vertexTexCoord :: TexCoord }
	deriving (Show, Eq, Ord, Generic)

newtype Pos = Pos Cglm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype Color = Color Cglm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance GStorable.G Vertex
