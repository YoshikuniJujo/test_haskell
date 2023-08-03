{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.WavefrontObj.Read (

	-- * FUNCTIONS

	countV,
	
	readPosTxt, readPosNormal, readPosTxtNormal,

	facePosTxt, facePosNormal, facePosTxtNormal,

	-- * COUNT

	Count(..),

	-- * FACE, POSITION, TEX COORD AND NORMAL

	Face(..), Position(..), TexCoord(..), Normal(..),

	-- * POSITION NORMAL

	PositionNormal(..), PositionTxtNormal(..),

	-- * UNCLASSIFIED

	uncurry3

	) where

import GHC.Generics
import Foreign.Storable.SizeAlignment
import Control.Monad.ST
import Control.Monad.Trans.Writer.CPS
import Data.STRef
import Data.Maybe
import Data.Word

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified Data.ByteString as BS
import qualified Foreign.Storable.Generic as GStorable

import qualified Codec.WavefrontObj.Scan as Scan

countV :: BS.ByteString -> Count
countV = snd . runWriter . Scan.s_ @_ @Word32 \case
	Scan.V _ _ _ -> tell $ mempty { countVertex = 1 }
	Scan.Vt _ _ -> tell $ mempty { countTexture = 1 }
	Scan.Vn _ _ _ -> tell $ mempty { countNormal = 1 }
	Scan.F _ _ _ -> tell $ mempty { countFace = 1 }
	Scan.F4 _ _ _ _ -> tell $ mempty { countFace = 2 }
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
instance GStorable.G Position

data TexCoord = TexCoord Float Float deriving (Show, Generic)

instance SizeAlignmentList TexCoord
instance GStorable.G TexCoord

data Normal = Normal Float Float Float deriving (Show, Generic)

instance SizeAlignmentList Normal
instance GStorable.G Normal

data Face = Face (W Indices) (W Indices) (W Indices) deriving (Show, Generic)

instance SizeAlignmentList Face
instance GStorable.G Face

data Indices = Indices Int Int Int deriving (Show, Generic)

instance SizeAlignmentList Indices
instance GStorable.G Indices

indicesToIndices :: Scan.Vertex Int -> W Indices
indicesToIndices (Scan.Vertex p t n) = GStorable.W $ Indices p (fromMaybe 0 t) (fromMaybe 0 n)

readPosTxtNormal :: Count -> BS.ByteString -> (
	V.Vector (W Position), V.Vector (W TexCoord),
	V.Vector (W Normal), V.Vector (W Face) )
readPosTxtNormal Count {
	countVertex = cv, countTexture = ct,
	countNormal = cn, countFace = cf } = readV' cv ct cn cf

readV' :: Int -> Int -> Int -> Int -> BS.ByteString ->
	(V.Vector (W Position), V.Vector (W TexCoord), V.Vector (W Normal), V.Vector (W Face))
readV' nv nt nn nf str = runST do
	iv <- newSTRef 0
	it <- newSTRef 0
	inml <- newSTRef 0
	ifc <- newSTRef 0
	vv <- MV.new nv
	t <- MV.new nt
	n <- MV.new nn
	f <- MV.new nf
	flip (Scan.s_ @_ @Int) str \case
		Scan.V x y z -> do
			i <- readSTRef iv
			MV.write vv i . GStorable.W $ Position x y z
			writeSTRef iv (i + 1)
		Scan.Vt u v -> do
			i <- readSTRef it
			MV.write t i . GStorable.W $ TexCoord u v
			writeSTRef it (i + 1)
		Scan.Vn x y z -> do
			i <- readSTRef inml
			MV.write n i . GStorable.W $ Normal x y z
			writeSTRef inml (i + 1)
		Scan.F i1 i2 i3 -> do
			i <- readSTRef ifc
			MV.write f i . GStorable.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i2)
				(indicesToIndices i3)
			writeSTRef ifc (i + 1)
		Scan.F4 i1 i2 i3 i4 -> do
			i <- readSTRef ifc
			MV.write f i . GStorable.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i2)
				(indicesToIndices i3)
			MV.write f (i + 1) . GStorable.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i3)
				(indicesToIndices i4)
			writeSTRef ifc (i + 2)
		_ -> pure ()
	(,,,) <$> V.freeze vv <*> V.freeze t <*> V.freeze n <*> V.freeze f

readPosNormal :: Count -> BS.ByteString -> (
	V.Vector (W Position), V.Vector (W Normal), V.Vector (W Face) )
readPosNormal Count { countVertex = cv, countNormal = cn, countFace = cf } =
	readVOld cv cn cf

readVOld :: Int -> Int -> Int -> BS.ByteString -> (
	V.Vector (W Position), V.Vector (W Normal), V.Vector (W Face) )
readVOld n n' n'' s = runST $ do
	ri <- newSTRef 0
	ri' <- newSTRef 0
	ri'' <- newSTRef 0
	v <- MV.new n
	t <- MV.new n'
	idx <- MV.new n''
	flip (Scan.s_ @_ @Int) s \case
		Scan.V x y z -> do
			i <- readSTRef ri
			MV.write v i . GStorable.W $ Position x y z
			writeSTRef ri (i + 1)
		Scan.Vn x y z -> do
			i' <- readSTRef ri'
			MV.write t i' . GStorable.W $ Normal x y z
			writeSTRef ri' (i' + 1)
		Scan.F idx1 idx2 idx3 -> do
			i'' <- readSTRef ri''
			MV.write idx i''
				. GStorable.W $ Face
					(indicesToIndices idx1)
					(indicesToIndices idx2)
					(indicesToIndices idx3)
			writeSTRef ri'' (i'' + 1)
		Scan.F4 i1 i2 i3 i4 -> do
			i'' <- readSTRef ri''
			MV.write idx i'' . GStorable.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i2)
				(indicesToIndices i3)
			MV.write idx (i'' + 1) . GStorable.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i3)
				(indicesToIndices i4)
			writeSTRef ri'' (i'' + 2)
		_ -> pure ()
	(,,) <$> V.freeze v <*> V.freeze t <*> V.freeze idx

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
	flip (Scan.s_ @_ @Int) s \case
		Scan.V x y z -> do
			i <- readSTRef ri
			MV.write v i . GStorable.W $ Position x y z
			writeSTRef ri (i + 1)
		Scan.Vt x y -> do
			i' <- readSTRef ri'
			MV.write t i' . GStorable.W $ TexCoord x (1 - y)
			writeSTRef ri' (i' + 1)
		Scan.F idx1 idx2 idx3 -> do
			i'' <- readSTRef ri''
			MV.write idx i''
				. GStorable.W $ Face
					(indicesToIndices idx1)
					(indicesToIndices idx2)
					(indicesToIndices idx3)
			writeSTRef ri'' (i'' + 1)
		_ -> pure ()
	(,,) <$> V.freeze v <*> V.freeze t <*> V.freeze idx

facePosNormal ::
	V.Vector (W Position) -> V.Vector (W Normal) -> V.Vector (W Face) ->
	V.Vector (W PositionNormal)
facePosNormal ps ns = indexPosNormal ps ns . loosenFace

indexPosNormal ::
	V.Vector (W Position) -> V.Vector (W Normal) -> V.Vector (W Indices) ->
	V.Vector (W PositionNormal)
indexPosNormal ps ns is = V.generate ln \i -> let
	GStorable.W (Indices iv _ inml) = is V.! i
	in
	GStorable.W $ PositionNormal (ps V.! (iv - 1)) (ns V.! (inml - 1))
	where ln = V.length is

data PositionNormal = PositionNormal {
	positionNormalPosition :: W Position,
	positionNormalNormal :: W Normal }
	deriving (Show, Generic)

instance SizeAlignmentList PositionNormal
instance GStorable.G PositionNormal

facePosTxtNormal ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> V.Vector (W Normal) -> V.Vector (W Face) ->
	V.Vector (W PositionTxtNormal)
facePosTxtNormal ps ts ns = indexPosTxtNormal ps ts ns . loosenFace

indexPosTxtNormal ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> V.Vector (W Normal) -> V.Vector (W Indices) ->
	V.Vector (W PositionTxtNormal)
indexPosTxtNormal ps ts ns is = V.generate ln \i -> let
	GStorable.W (Indices iv it inml) = is V.! i
	in
	GStorable.W $ PositionTxtNormal (ps V.! (iv - 1)) (ts V.! (it - 1)) (ns V.! (inml - 1))
	where ln = V.length is

data PositionTxtNormal = PositionTxtNormal {
	positionTxtNormalPosition :: W Position,
	positionTxtNormalTxt :: W TexCoord,
	positionTxtNormalNormal :: W Normal }
	deriving (Show, Generic)

instance SizeAlignmentList PositionTxtNormal
instance GStorable.G PositionTxtNormal

type W = GStorable.Wrap

loosenFace :: V.Vector (W Face) -> V.Vector (W Indices)
loosenFace fs = V.generate ln \i -> let
	GStorable.W (Face is0 is1 is2) = fs V.! (i `div` 3) in
	case i `mod` 3 of 0 -> is0; 1 -> is1; 2 -> is2; _ -> error "never occur"
	where ln = 3 * V.length fs

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

readPosTxt :: Count -> BS.ByteString -> (
	V.Vector (W Position), V.Vector (W TexCoord), V.Vector (W Face) )
readPosTxt Count { countVertex = n, countTexture = n', countFace = n'' } bs =
	runST $ readV n n' n'' bs

facePosTxt ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> V.Vector (W Face) ->
	V.Vector (W (W Position, W TexCoord))
facePosTxt ps ts = indexPosTxt ps ts . loosenFace

indexPosTxt ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> V.Vector (W Indices) ->
	V.Vector (W (W Position, W TexCoord))
indexPosTxt ps ts is = V.generate ln \i -> let
	GStorable.W i' = is V.! i in GStorable.W $ indicesToPosTex' ps ts i'
	where ln = V.length is

indicesToPosTex' ::
	V.Vector (W Position) -> V.Vector (W TexCoord) -> Indices ->
	(W Position, W TexCoord)
indicesToPosTex' ps ts (Indices ip it _) = (ps V.! (ip - 1), ts V.! (it - 1) )
