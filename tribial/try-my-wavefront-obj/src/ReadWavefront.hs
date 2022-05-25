{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ReadWavefront where

import GHC.Generics
import Foreign.Storable.SizeAlignment
import Control.Monad.ST
import Control.Monad.Writer
import Data.STRef
import Data.Maybe
import Data.Word

import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.ByteString as BS
import qualified Foreign.Storable.Generic

import qualified Wavefront as Wf
import qualified Vertex as Vtx

countV :: BS.ByteString -> Writer (Sum Int, Sum Int, Sum Int, Sum Int) ()
countV = Wf.parseWavefront_ @_ @Word32 \case
	Wf.V _ _ _ -> tell (1, 0, 0, 0)
	Wf.Vt _ _ -> tell (0, 1, 0, 0)
	Wf.Vn _ _ _ -> tell (0, 0, 1, 0)
	Wf.F _ _ _ -> tell (0, 0, 0, 1)
	_ -> tell (0, 0, 0, 0)

data Position = Position Double Double Double deriving (Show, Generic)

instance SizeAlignmentList Position
instance Foreign.Storable.Generic.G Position

data TexCoord = TexCoord Double Double deriving (Show, Generic)

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

readV :: Int -> Int -> Int -> BS.ByteString -> ST s (
	V.Vector (W Position), V.Vector (W TexCoord),
	V.Vector (W Face))
readV n n' n'' s = do
	ri <- newSTRef 0
	ri' <- newSTRef 0
	ri'' <- newSTRef 0
	v <- V.new n
	t <- V.new n'
	idx <- V.new n''
	flip (Wf.parseWavefront_ @_ @Int) s \case
		Wf.V x y z -> do
			i <- readSTRef ri
			V.write v i . w $ Position x y z
			writeSTRef ri (i + 1)
		Wf.Vt x y -> do
			i' <- readSTRef ri'
			V.write t i' . w $ TexCoord x y
			writeSTRef ri' (i' + 1)
		Wf.F idx1 idx2 idx3 -> do
			i'' <- readSTRef ri''
			V.write idx i''
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

w :: a -> W a
w = Foreign.Storable.Generic.Wrap
