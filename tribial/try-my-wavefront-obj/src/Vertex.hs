{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex where

import GHC.Generics
import Foreign.Storable
import Data.Word

import qualified Data.Vector.Storable as V
import qualified Foreign.Storable.Generic
import qualified Cglm

data Vertex = Vertex {
	vertexPos :: Cglm.Vec3,
	vertexColor :: Color,
	vertexTexCoord :: TexCoord }
	deriving (Show, Generic)

newtype Color = Color Cglm.Vec3
	deriving (Show, Storable)

newtype TexCoord = TexCoord Cglm.Vec2
	deriving (Show, Storable)

type WVertex = Foreign.Storable.Generic.Wrap Vertex

type Vertices = V.Vector WVertex

type WWord32 = Foreign.Storable.Generic.Wrap Word32

type Indices = V.Vector WWord32
