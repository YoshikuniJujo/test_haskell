{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex where

import GHC.Generics
import Foreign.Storable

import qualified Gpu.Vulkan.Pipeline.VertexInputState as Vk.Ppl.VertexInputSt

import Gpu.Vulkan.Cglm qualified as Cglm
import qualified Foreign.Storable.Generic as GStorable

type WVertex = GStorable.W Vertex

data Vertex = Vertex {
	vertexPos :: Pos, vertexColor :: Color, vertexTexCoord :: TexCoord }
	deriving (Show, Eq, Ord, Generic)

newtype Pos = Pos Cglm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)

instance GStorable.G Vertex

newtype Color = Color Cglm.Vec3
	deriving (Show, Eq, Ord, Storable, Vk.Ppl.VertexInputSt.Formattable)
