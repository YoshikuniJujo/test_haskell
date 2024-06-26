{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Wavefront.Read (readVertices, readSample) where

import GHC.Generics
import Foreign.Storable.SizeAlignment
import Control.Monad.ST
import Control.Monad.Writer
import Data.List.Length
import Data.STRef
import Data.Maybe
import Data.Word

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.ByteString as BS
import qualified Foreign.Storable.Generic

import qualified Codec.Wavefront.Parse as Wf
import qualified Vertex as Vtx
import qualified Cglm

readSample :: IO BS.ByteString
readSample = BS.readFile "../../files/models/viking_room.obj"

countV :: BS.ByteString -> Writer (Sum Int, Sum Int, Sum Int, Sum Int) ()
countV = Wf.parseWavefront_ @_ @Word32 \case
	Wf.V _ _ _ -> tell (1, 0, 0, 0)
	Wf.Vt _ _ -> tell (0, 1, 0, 0)
	Wf.Vn _ _ _ -> tell (0, 0, 1, 0)
	Wf.F _ _ _ -> tell (0, 0, 0, 1)
	_ -> tell (0, 0, 0, 0)

data Position = Position Float Float Float deriving (Show, Generic)

instance SizeAlignmentList Position
instance Foreign.Storable.Generic.G Position

data TexCoord = TexCoord Float Float deriving (Show, Generic)

instance SizeAlignmentList TexCoord
instance Foreign.Storable.Generic.G TexCoord

data Face = Face (W Indices) (W Indices) (W Indices) deriving (Show, Generic)

instance SizeAlignmentList Face
instance Foreign.Storable.Generic.G Face

data Indices = Indices Int Int Int deriving (Show, Generic)

instance SizeAlignmentList Indices
instance Foreign.Storable.Generic.G Indices

indicesToIndices :: Wf.Vertex Int -> W Indices
indicesToIndices (Wf.Vertex p t n) = w $ Indices p (fromMaybe 0 t) (fromMaybe 0 n)

indicesToPosTex ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> Indices ->
	(Position, TexCoord)
indicesToPosTex ps ts (Indices ip it _) = (
	uw $ ps V.! (ip - 1),
	uw $ ts V.! (it - 1) )

readV :: Int -> Int -> Int -> BS.ByteString -> ST s (
	V.Vector (W Position), V.Vector (W TexCoord),
	V.Vector (W Face))
readV n n' n'' s = do
	ri <- newSTRef 0
	ri' <- newSTRef 0
	ri'' <- newSTRef 0
	v <- MV.new n
	t <- MV.new n'
	idx <- MV.new n''
	flip (Wf.parseWavefront_ @_ @Int) s \case
		Wf.V x y z -> do
			i <- readSTRef ri
			MV.write v i . w $ Position x y z
			writeSTRef ri (i + 1)
		Wf.Vt x y -> do
			i' <- readSTRef ri'
			MV.write t i' . w $ TexCoord x y
			writeSTRef ri' (i' + 1)
		Wf.F idx1 idx2 idx3 -> do
			i'' <- readSTRef ri''
			MV.write idx i''
				. w $ Face
					(indicesToIndices idx1)
					(indicesToIndices idx2)
					(indicesToIndices idx3)
			writeSTRef ri'' (i'' + 1)
		_ -> pure ()
	(,,) <$> V.freeze v <*> V.freeze t <*> V.freeze idx

readVertexPositions :: BS.ByteString -> (V.Vector (W Position), V.Vector (W TexCoord), V.Vector (W Face))
readVertexPositions bs = let
	((), (Sum n, Sum n', _, Sum n'')) = runWriter $ countV bs in
	runST $ readV n n' n'' bs

type W = Foreign.Storable.Generic.Wrap

{-# COMPLETE W #-}

pattern W :: a -> Foreign.Storable.Generic.Wrap a
pattern W a <- Foreign.Storable.Generic.Wrap a

w :: a -> W a
w = Foreign.Storable.Generic.Wrap

uw :: W a -> a
uw = Foreign.Storable.Generic.unWrap

loosenFace :: V.Vector (W Face) -> V.Vector (W Indices)
loosenFace fs = V.generate ln \i -> let
	W (Face is0 is1 is2) = fs V.! (i `div` 3) in
	case i `mod` 3 of 0 -> is0; 1 -> is1; 2 -> is2; _ -> error "never occur"
	where ln = 3 * V.length fs

getVertices ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> V.Vector (W Indices) ->
	V.Vector Vtx.WVertex
getVertices ps ts is = V.generate ln \i ->
	let	W ids = is V.! i
		(Position x y z, TexCoord u v) = indicesToPosTex ps ts ids in
	w $ Vtx.Vertex
		(Cglm.Vec3 $ x :. y :. z :. NilL)
		(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
		(Vtx.TexCoord . Cglm.Vec2 $ u :. v :. NilL)
	where ln = V.length is

readVertices :: BS.ByteString -> V.Vector Vtx.WVertex
readVertices bs = let
	(ps, ts, fs) = readVertexPositions bs
	is = loosenFace fs in
	getVertices ps ts is
