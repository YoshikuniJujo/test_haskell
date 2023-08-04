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
	FilePath -> IO (V.Vector (GStorable.W Vtx.Vertex), V.Vector Word32)
verticesIndices fp =
	either error pure =<< readVerticesIndices <$> BS.readFile fp
	where
	readVerticesIndices :: BS.ByteString ->
		Either String (V.Vector (GStorable.W Vtx.Vertex), V.Vector Word32)
	readVerticesIndices bs =
		(\vs -> (vs, makeIndices vs)) <$> readVertices bs
	makeIndices :: V.Vector (GStorable.W Vtx.Vertex) -> V.Vector Word32
	makeIndices vs = V.generate (V.length vs) \i -> fromIntegral i

type W = GStorable.W

readVertices :: BS.ByteString -> Either String (V.Vector (GStorable.W Vtx.Vertex))
readVertices bs =
	V.map posTexToVertex <$> uncurry3 facePosTex (readPosTex (countV bs) bs)
	where
	posTexToVertex :: W (W Position, W TexCoord) -> W Vtx.Vertex
	posTexToVertex (GStorable.W (
		GStorable.W (Position x y z),
		GStorable.W (TexCoord u v) )) = GStorable.W $ Vtx.Vertex
		(Vtx.Pos . Cglm.Vec3 $ x :. y :. z :. NilL)
		(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
		(Vtx.TexCoord . Cglm.Vec2 $ u :. v :. NilL)
