{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex.Wavefront (

	-- * FUNCTIONS

	readVertices

	) where

import Foreign.Storable.Generic qualified as GStorable
import Data.List.Length
import Data.Vector.Storable qualified as V
import Data.ByteString qualified as BS
import Codec.WavefrontObj.ReadFaceSimple qualified as Wf.Read

import Gpu.Vulkan.Cglm qualified as Cglm
import Vertex qualified as Vtx


readVertices :: FilePath -> IO (V.Vector (GStorable.W Vtx.Vertex))
readVertices fp = either error pure =<< vertices <$> BS.readFile fp

vertices :: BS.ByteString -> Either String (V.Vector (GStorable.W Vtx.Vertex))
vertices bs = V.map tov <$> Wf.Read.posTex (Wf.Read.r bs)
	where tov (GStorable.W (
		GStorable.W (Wf.Read.Position x y z),
		GStorable.W (Wf.Read.TexCoord u v) )) = GStorable.W $ Vtx.Vertex
		(Vtx.Pos . Cglm.Vec3 $ x :. y :. z :. NilL)
		(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
		(Vtx.TexCoord . Cglm.Vec2 $ u :. (1 - v) :. NilL)
