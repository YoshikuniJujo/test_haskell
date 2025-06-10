{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.Sequence qualified as Seq
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png

import Data.Color
import Data.Png.Header qualified as Header

import Data.Bits
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String . Fail.runExc id
		. Png.run_ . Pipe.run
		. (`Except.catch` IO.putStrLn) . void
		$ PipeBS.hGet (32 * 32) h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$=
			Png.decode IO.print IO.print Pipe.=$=
			bytesToColor Pipe.=$=
			PipeIO.print

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

bytesToColor :: (
	RealFrac d,
	U.Member Pipe.P es,
	U.Member (State.Named "foobar" Header.Header) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es [Word8] (Either [Rgb d] [Rgba d]) r
bytesToColor = do
	bs1 <- Pipe.await
	(ct, bd) <- maybe (Except.throw @String "yet") pure . headerToColorTypeDepth =<< State.getN "foobar"
	case ct of
		Rgb -> do
			Pipe.yield . Left $ pixelsRgb bd bs1
			PipeT.convert (Left . pixelsRgb bd)
		Rgba -> do
			Pipe.yield . Right $ pixelsRgba bd bs1
			PipeT.convert (Right . pixelsRgba bd)

data ColorType = Rgb | Rgba deriving Show
data BitDepth = BitDepth8 | BitDepth16 deriving Show

headerToColorTypeDepth :: Header.Header -> Maybe (ColorType, BitDepth)
headerToColorTypeDepth h = do
	ct <- case Header.headerColorType h of
		Header.ColorTypeColorUsed -> Just Rgb
		Header.ColorType 6 -> Just Rgba
		_ -> Nothing
	bd <- case Header.headerBitDepth h of
		8 -> Just BitDepth8
		16 -> Just BitDepth16
		_ -> Nothing
	pure (ct, bd)

pixelsRgb :: RealFrac d => BitDepth -> [Word8] -> [Rgb d]
pixelsRgb bd bs = case samples bd bs of
	Left ss -> colorsRgb RgbWord8 ss
	Right ss -> colorsRgb RgbWord16 ss

pixelsRgba :: RealFrac d => BitDepth -> [Word8] -> [Rgba d]
pixelsRgba bd bs = case samples bd bs of
	Left ss -> colorsRgba RgbaWord8 ss
	Right ss -> colorsRgba RgbaWord16 ss

pixels :: RealFrac d => ColorType -> BitDepth -> [Word8] -> Either [Rgb d] [Rgba d]
pixels ct bd bs = case samples bd bs of
	Left ss -> colors ct RgbWord8 RgbaWord8 ss
	Right ss -> colors ct RgbWord16 RgbaWord16 ss

samples :: BitDepth -> [Word8] -> Either [Word8] [Word16]
samples BitDepth8 bs = Left bs
samples BitDepth16 [] = Right []
samples BitDepth16 ((fromIntegral -> b) : (fromIntegral -> b') : bs) =
	((b `shiftL` 8 .|. b') :) <$> samples BitDepth16 bs
samples BitDepth16 [_] = error "bad"

colors :: ColorType ->
	(s -> s -> s -> Rgb d) -> (s -> s -> s -> s -> Rgba d) -> [s] ->
	Either [Rgb d] [Rgba d]
colors Rgb rgb _ = Left . colorsRgb rgb
colors Rgba _ rgba = Right . colorsRgba rgba	

colorsRgb :: (s -> s -> s -> Rgb d) -> [s] -> [Rgb d]
colorsRgb rgb = \case
	[] -> []
	r : g : b : ss -> rgb r g b : colorsRgb rgb ss
	_ -> error "bad"

colorsRgba :: (s -> s -> s -> s -> Rgba d) -> [s] -> [Rgba d]
colorsRgba rgba = \case
	[] -> []
	r : g : b : a : ss -> rgba r g b a : colorsRgba rgba ss
	_ -> error "bad"
