{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.WavefrontObj.ReadSimple (

	-- * READ

	r, Result(..),

	-- * POSITIONS, TEXTURE COORDINATES AND NORMALS

	pos, posTex, posNormal, posTexNormal,
	Position(..), TexCoord(..), Normal(..),

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
import qualified Foreign.Storable.Generic as GStr

import qualified Codec.WavefrontObj.Scan as Scan

r :: BS.ByteString -> Result
r = readPosTexNormal <$> countV <*> id

data Result = Result {
	resultPositions :: V.Vector (GStr.W Position),
	resultTexCoords :: V.Vector (GStr.W TexCoord),
	resultNormals :: V.Vector (GStr.W Normal),
	resultFaces :: V.Vector (GStr.W Face) }
	deriving Show

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
instance GStr.G Position

data TexCoord = TexCoord Float Float deriving (Show, Generic)

instance SizeAlignmentList TexCoord
instance GStr.G TexCoord

data Normal = Normal Float Float Float deriving (Show, Generic)

instance SizeAlignmentList Normal
instance GStr.G Normal

data Face = Face (GStr.W Indices) (GStr.W Indices) (GStr.W Indices) deriving (Show, Generic)

instance SizeAlignmentList Face
instance GStr.G Face

data Indices = Indices Int Int Int deriving (Show, Generic)

instance SizeAlignmentList Indices
instance GStr.G Indices

indicesToIndices :: Scan.Vertex Int ->GStr.W Indices
indicesToIndices (Scan.Vertex p t n) = GStr.W $ Indices p (fromMaybe 0 t) (fromMaybe 0 n)

readPosTexNormal :: Count -> BS.ByteString -> Result
readPosTexNormal Count {
	countVertex = cv, countTexture = ct,
	countNormal = cn, countFace = cf } = readV' cv ct cn cf

readV' :: Int -> Int -> Int -> Int -> BS.ByteString -> Result
--	(V.Vector (GStr.W Position), V.Vector (GStr.W TexCoord), V.Vector (GStr.W Normal), V.Vector (GStr.W Face))
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
			MV.write vv i . GStr.W $ Position x y z
			writeSTRef iv (i + 1)
		Scan.Vt u v -> do
			i <- readSTRef it
			MV.write t i . GStr.W $ TexCoord u v
			writeSTRef it (i + 1)
		Scan.Vn x y z -> do
			i <- readSTRef inml
			MV.write n i . GStr.W $ Normal x y z
			writeSTRef inml (i + 1)
		Scan.F i1 i2 i3 -> do
			i <- readSTRef ifc
			MV.write f i . GStr.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i2)
				(indicesToIndices i3)
			writeSTRef ifc (i + 1)
		Scan.F4 i1 i2 i3 i4 -> do
			i <- readSTRef ifc
			MV.write f i . GStr.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i2)
				(indicesToIndices i3)
			MV.write f (i + 1) . GStr.W $ Face
				(indicesToIndices i1)
				(indicesToIndices i3)
				(indicesToIndices i4)
			writeSTRef ifc (i + 2)
		_ -> pure ()
	Result <$> V.freeze vv <*> V.freeze t <*> V.freeze n <*> V.freeze f

posNormal ::
	Result ->
	Either String (V.Vector (GStr.W (GStr.W Position, GStr.W Normal)))
posNormal
	Result { resultPositions = ps, resultNormals = ns, resultFaces = fs } =
	indexPosNormal ps ns $ loosenFace fs

indexPosNormal ::
	V.Vector (GStr.W Position) -> V.Vector (GStr.W Normal) ->
	V.Vector (GStr.W Indices) ->
	Either String (V.Vector (GStr.W (GStr.W Position, GStr.W Normal)))
indexPosNormal ps ns is = V.generateM ln \i -> case is V.! i of
	GStr.W (Indices _ _ 0) -> Left "There is the vertex which has no normals."
	GStr.W (Indices iv _ inml) ->
		Right . GStr.W $ (ps V.! (iv - 1), ns V.! (inml - 1))
	where ln = V.length is

posTexNormal :: Result -> Either String (
	V.Vector (GStr.W (GStr.W Position, GStr.W TexCoord, GStr.W Normal)) )
posTexNormal Result {
	resultPositions = ps, resultTexCoords = ts,
	resultNormals = ns, resultFaces = fs } =
	indexPosTexNormal ps ts ns $ loosenFace fs

indexPosTexNormal ::
	V.Vector (GStr.W Position) -> V.Vector (GStr.W TexCoord) ->
	V.Vector (GStr.W Normal) -> V.Vector (GStr.W Indices) ->
	Either String (V.Vector (GStr.W (GStr.W Position, GStr.W TexCoord, GStr.W Normal)))
indexPosTexNormal ps ts ns is = V.generateM ln \i -> case is V.! i of
	GStr.W (Indices _ 0 0) -> Left $
		"There is the vertex which has no texture coordinates or " ++
		"has no normals"
	GStr.W (Indices iv it inml) ->
		Right . GStr.W $ (,,)
			(ps V.! (iv - 1)) (ts V.! (it - 1)) (ns V.! (inml - 1))
	where ln = V.length is

loosenFace :: V.Vector (GStr.W Face) -> V.Vector (GStr.W Indices)
loosenFace fs = V.generate ln \i -> let
	GStr.W (Face is0 is1 is2) = fs V.! (i `div` 3) in
	case i `mod` 3 of 0 -> is0; 1 -> is1; 2 -> is2; _ -> error "never occur"
	where ln = 3 * V.length fs

pos :: Result -> Either String (V.Vector (GStr.W Position))
pos Result { resultPositions = ps, resultFaces = fs } =
	indexPos ps $ loosenFace fs

indexPos ::
	V.Vector (GStr.W Position) -> V.Vector (GStr.W Indices) ->
	Either String (V.Vector (GStr.W Position))
indexPos ps is = V.generateM ln \i -> let
	GStr.W i' = is V.! i in indicesToPos' ps i'
	where ln = V.length is

indicesToPos' ::
	V.Vector (GStr.W Position) -> Indices -> Either String (GStr.W Position)
indicesToPos' ps = \case
	(Indices _ 0 _) ->
		Left "There is the vertex which has no texture coordinate."
	(Indices ip it _) -> Right (ps V.! (ip - 1))

posTex :: Result ->
	Either String (V.Vector (GStr.W (GStr.W Position, GStr.W TexCoord)))
posTex Result {
	resultPositions = ps, resultTexCoords = ts, resultFaces = fs } =
	indexPosTex ps ts $ loosenFace fs

indexPosTex ::
	V.Vector (GStr.W Position) -> V.Vector (GStr.W TexCoord) -> V.Vector (GStr.W Indices) ->
	Either String (V.Vector (GStr.W (GStr.W Position, GStr.W TexCoord)))
indexPosTex ps ts is = V.generateM ln \i -> let
	GStr.W i' = is V.! i in
	GStr.W <$> indicesToPosTex' ps ts i'
	where ln = V.length is

indicesToPosTex' ::
	V.Vector (GStr.W Position) -> V.Vector (GStr.W TexCoord) -> Indices ->
	Either String (GStr.W Position, GStr.W TexCoord)
indicesToPosTex' ps ts = \case
	(Indices _ 0 _) ->
		Left "There is the vertex which has no texture coordinate."
	(Indices ip it _) -> Right (ps V.! (ip - 1), ts V.! (it - 1) )
