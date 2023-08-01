{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Wavefront.Read (

	readSample,
	verticesIndices',

	Position(..), Normal(..),

	countV, countV', Count(..), readV', takePosNormalFace, facePosNormal, PositionNormal(..) ) where

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
import Gpu.Vulkan.Cglm qualified as Cglm

verticesIndices' :: FilePath -> IO (V.Vector Vtx.Vertex, V.Vector Word32)
verticesIndices' fp = readVerticesIndices' <$> BS.readFile fp

readSample, readTiny :: IO BS.ByteString
readSample = BS.readFile "../../../../files/models/viking_room.obj"
readTiny = BS.readFile "tiny.obj"

countV :: BS.ByteString -> Writer (Sum Int, Sum Int, Sum Int, Sum Int) ()
countV = Wf.parseWavefront_ @_ @Word32 \case
	Wf.V _ _ _ -> tell (1, 0, 0, 0)
	Wf.Vt _ _ -> tell (0, 1, 0, 0)
	Wf.Vn _ _ _ -> tell (0, 0, 1, 0)
	Wf.F _ _ _ -> tell (0, 0, 0, 1)
	_ -> tell (0, 0, 0, 0)

countV' :: BS.ByteString -> Count
countV' = snd . runWriter . Wf.parseWavefront_ @_ @Word32 \case
	Wf.V _ _ _ -> tell $ mempty { countVertex = 1 }
	Wf.Vt _ _ -> tell $ mempty { countTexture = 1 }
	Wf.Vn _ _ _ -> tell $ mempty { countNormal = 1 }
	Wf.F _ _ _ -> tell $ mempty { countFace = 1 }
	_ -> tell mempty

data Count = Count {
	countVertex :: Int, countTexture :: Int,
	countNormal :: Int, countFace :: Int }
	deriving Show

instance Semigroup Count where
	c1 <> c2 = Count {
		countVertex = countVertex c1 + countVertex c2,
		countTexture = countTexture c1 + countTexture c2,
		countNormal = countNormal c1 + countNormal c2,
		countFace = countFace c1 + countFace c2 }

instance Monoid Count where
	mempty = Count {
		countVertex = 0, countTexture = 0,
		countNormal = 0, countFace = 0 }

data Position = Position Float Float Float deriving (Show, Generic)

instance SizeAlignmentList Position
instance Foreign.Storable.Generic.G Position

data TexCoord = TexCoord Float Float deriving (Show, Generic)

instance SizeAlignmentList TexCoord
instance Foreign.Storable.Generic.G TexCoord

data Normal = Normal Float Float Float deriving (Show, Generic)

instance SizeAlignmentList Normal
instance Foreign.Storable.Generic.G Normal

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
			MV.write t i' . w $ TexCoord x (1 - y)
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

readV' :: Int -> Int -> Int -> BS.ByteString ->
	(V.Vector (W Position), V.Vector (W Normal), V.Vector (W Face))
readV' nv nn nf str = runST do
	iv <- newSTRef 0
	inml <- newSTRef 0
	ifc <- newSTRef 0
	v <- MV.new nv
	n <- MV.new nn
	f <- MV.new nf
	flip (Wf.parseWavefront_ @_ @Int) str \case
		Wf.V x y z -> do
			i <- readSTRef iv
			MV.write v i . W $ Position x y z
			writeSTRef iv (i + 1)
		Wf.Vn x y z -> do
			i <- readSTRef inml
			MV.write n i . W $ Normal x y z
			writeSTRef inml (i + 1)
		Wf.F i1 i2 i3 -> do
			i <- readSTRef ifc
			MV.write f i . W $ Face
				(indicesToIndices i1)
				(indicesToIndices i2)
				(indicesToIndices i3)
			writeSTRef ifc (i + 1)
		_ -> pure ()
	(,,) <$> V.freeze v <*> V.freeze n <*> V.freeze f

facePosNormal ::
	V.Vector (W Position) -> V.Vector (W Normal) -> V.Vector (W Face) ->
	V.Vector (W PositionNormal)
facePosNormal ps ns = indexPosNormal ps ns . loosenFace

indexPosNormal ::
	V.Vector (W Position) -> V.Vector (W Normal) -> V.Vector (W Indices) ->
	V.Vector (W PositionNormal)
indexPosNormal ps ns is = V.generate ln \i -> let
	W (Indices iv _ inml) = is V.! i
	in
	W $ PositionNormal (ps V.! (iv - 1)) (ns V.! (inml - 1))
	where ln = V.length is

data PositionNormal = PositionNormal {
	positionNormalPosition :: W Position,
	positionNormalNormal :: W Normal }
	deriving (Show, Generic)

instance SizeAlignmentList PositionNormal
instance Foreign.Storable.Generic.G PositionNormal

takePosNormalFace :: Int ->
	(V.Vector (W Position), V.Vector (W Normal), V.Vector (W Face)) ->
	(V.Vector (W Position), V.Vector (W Normal), V.Vector (W Face))
takePosNormalFace n (vs, ns, fs) = (V.take n vs, V.take n ns, V.take n fs)

readVertexPositions :: BS.ByteString -> (V.Vector (W Position), V.Vector (W TexCoord), V.Vector (W Face))
readVertexPositions bs = let
	((), (Sum n, Sum n', _, Sum n'')) = runWriter $ countV bs in
	runST $ readV n n' n'' bs

type W = Foreign.Storable.Generic.Wrap

{-# COMPLETE W #-}

pattern W :: a -> Foreign.Storable.Generic.Wrap a
pattern W a <- Foreign.Storable.Generic.Wrap a
	where W = Foreign.Storable.Generic.Wrap

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
	V.Vector Vtx.Vertex
getVertices ps ts is = V.generate ln \i ->
	let	W ids = is V.! i
		(Position x y z, TexCoord u v) = indicesToPosTex ps ts ids in
	Vtx.Vertex
		(Vtx.Pos . Cglm.Vec3 $ x :. y :. z :. NilL)
		(Vtx.Color . Cglm.Vec3 $ 1 :. 1 :. 1 :. NilL)
		(Vtx.TexCoord . Cglm.Vec2 $ u :. v :. NilL)
	where ln = V.length is

readVertices :: BS.ByteString -> V.Vector Vtx.Vertex
readVertices bs = let
	(ps, ts, fs) = readVertexPositions bs
	is = loosenFace fs in
	getVertices ps ts is

makeIndices' :: V.Vector Vtx.Vertex -> V.Vector Word32
makeIndices' vs = V.generate (V.length vs) \i -> fromIntegral i

readVerticesIndices' :: BS.ByteString -> (V.Vector Vtx.Vertex, V.Vector Word32)
readVerticesIndices' bs = let vs = readVertices bs in (vs, makeIndices' vs)
