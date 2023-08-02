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

import Control.Monad.ST
import Data.List.Length
import Data.Word

import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS
import qualified Foreign.Storable.Generic as GStorable

import qualified Vertex as Vtx
import Gpu.Vulkan.Cglm qualified as Cglm

import Codec.Wavefront.Read

verticesIndices :: FilePath -> IO (V.Vector (GStorable.Wrap Vtx.Vertex), V.Vector Word32)
verticesIndices fp = readVerticesIndices' <$> BS.readFile fp

readVertexPositions :: BS.ByteString -> (V.Vector (W Position), V.Vector (W TexCoord), V.Vector (W Face))
readVertexPositions bs = let
	Count { countVertex = n, countTexture = n', countFace = n'' } = countV bs in
	runST $ readV n n' n'' bs

indicesToPosTex ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> Indices ->
	(Position, TexCoord)
indicesToPosTex ps ts (Indices ip it _) = (
	GStorable.unW $ ps V.! (ip - 1),
	GStorable.unW $ ts V.! (it - 1) )

type W = GStorable.Wrap

loosenFace :: V.Vector (W Face) -> V.Vector (W Indices)
loosenFace fs = V.generate ln \i -> let
	GStorable.W (Face is0 is1 is2) = fs V.! (i `div` 3) in
	case i `mod` 3 of 0 -> is0; 1 -> is1; 2 -> is2; _ -> error "never occur"
	where ln = 3 * V.length fs

getVertices ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> V.Vector (W Indices) ->
	V.Vector (GStorable.Wrap Vtx.Vertex)
getVertices ps ts is = V.generate ln \i ->
	let	GStorable.W ids = is V.! i
		(Position x y z, TexCoord u v) = indicesToPosTex ps ts ids in
	GStorable.W $ Vtx.Vertex
		(Vtx.Pos . Cglm.Vec3 $ x :. y :. z :. NilL)
		(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
		(Vtx.TexCoord . Cglm.Vec2 $ u :. v :. NilL)
	where ln = V.length is

readVertices :: BS.ByteString -> V.Vector (GStorable.Wrap Vtx.Vertex)
readVertices bs = let
	(ps, ts, fs) = readVertexPositions bs
	is = loosenFace fs in
	getVertices ps ts is

makeIndices' :: V.Vector (GStorable.Wrap Vtx.Vertex) -> V.Vector Word32
makeIndices' vs = V.generate (V.length vs) \i -> fromIntegral i

readVerticesIndices' :: BS.ByteString ->
	(V.Vector (GStorable.Wrap Vtx.Vertex), V.Vector Word32)
readVerticesIndices' bs = let vs = readVertices bs in (vs, makeIndices' vs)
