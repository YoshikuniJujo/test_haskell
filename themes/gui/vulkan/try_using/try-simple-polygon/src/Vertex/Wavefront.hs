{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vertex.Wavefront (

	-- * FUNCTIONS

	readFile

	) where

import Prelude hiding (readFile)
import Foreign.Storable.Generic (W(..))
import Data.Vector.Storable qualified as V
import Data.List.Length
import Data.ByteString qualified as BS
import Codec.WavefrontObj.ReadFaceSimple qualified as WfR
import Gpu.Vulkan.Cglm qualified as Cglm

import Vertex qualified as Vtx

readFile :: FilePath -> IO (V.Vector (W Vtx.Vertex))
readFile fp = either error pure =<< parse <$> BS.readFile fp

parse :: BS.ByteString -> Either String (V.Vector (W Vtx.Vertex))
parse bs = V.map p2v <$> WfR.posTex (WfR.r bs)
	where p2v (W ( W (WfR.Position x y z), W (WfR.TexCoord u v) )) =
		W $ Vtx.Vertex
			(Vtx.Pos . Cglm.Vec3 $ x :. y :. z :. NilL)
			(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
			(Vtx.TexCoord . Cglm.Vec2 $ u :. (1 - v) :. NilL)
