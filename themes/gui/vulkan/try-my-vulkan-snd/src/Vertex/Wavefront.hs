{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex.Wavefront (

	-- * FUNCTIONS

	verticesIndices,

	) where

import Data.List.Length
import Data.Word

import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Foreign.Storable.Generic as GStorable

import qualified Vertex as Vtx
import Gpu.Vulkan.Cglm qualified as Cglm

import Codec.WavefrontObj.Read

verticesIndices ::
	FilePath -> IO (V.Vector (GStorable.Wrap Vtx.Vertex), V.Vector Word32)
verticesIndices fp = readVerticesIndices <$> BS.readFile fp
	where
	readVerticesIndices :: BS.ByteString ->
		(V.Vector (GStorable.Wrap Vtx.Vertex), V.Vector Word32)
	readVerticesIndices bs =
		let vs = readVertices bs in (vs, makeIndices vs)
	makeIndices :: V.Vector (GStorable.Wrap Vtx.Vertex) -> V.Vector Word32
	makeIndices vs = V.generate (V.length vs) \i -> fromIntegral i

type W = GStorable.Wrap

readVertices :: BS.ByteString -> V.Vector (GStorable.Wrap Vtx.Vertex)
readVertices bs =
	V.map posTxtToVertex . uncurry3 facePosTxt $ readPosTxt (countV bs) bs
	where
	posTxtToVertex :: W (W Position, W TexCoord) -> W Vtx.Vertex
	posTxtToVertex (GStorable.W (
		GStorable.W (Position x y z),
		GStorable.W (TexCoord u v) )) = GStorable.W $ Vtx.Vertex
		(Vtx.Pos . Cglm.Vec3 $ x :. y :. z :. NilL)
		(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
		(Vtx.TexCoord . Cglm.Vec2 $ u :. v :. NilL)
