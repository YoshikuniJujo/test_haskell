{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.ByteString.FingerTree qualified as BSF
import Data.Color
import Data.Image.Simple qualified as Image
import Data.Png.Header qualified as Header

import System.IO
import System.Environment
import System.FilePath

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Control.Monad.Yaftee.Pipe.Apng.Decode

import "try-gzip-yaftee-zlib-ft" Tools


import Data.IORef

import Data.Word

import FctlImage qualified
import System.File.Apng

main :: IO ()
main = do
	fp : fpo : _ <- getArgs

	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		. void $ PipeBS.hGet 32 hh
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Png.decodeHeader "foobar"
	hClose hh

	rfn <- newIORef 0
	imgs <- newIORef []
	rfctls <- newIORef []

	print hdr

	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Fail.runExc id id
		. apngRun_ @"foobar"
		. Pipe.run
		. (`Fail.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= apngPipe "foobar" hdr ibd obd
			Pipe.=$= do
				fctl0 <- firstFctl
				Pipe.yield []
				whileWithMeta (writeImage1 rfctls imgs) fctl0
			Pipe.=$= do
				[] <- Pipe.await
				IO.print @FrameNumber =<< State.getN "foobar"
				FrameNumber fn <- State.getN "foobar"

				for_ [0 .. fn - 1] \n -> do

					[] <- Pipe.await
					fctl0 <- (!! n) <$> Eff.effBase (readIORef rfctls)
					pipeZip $ (n ,) <$> (fctlPoss hdr fctl0)

			Pipe.=$= forever do
				clrs <- Pipe.await
				(\(clr, (n, (x, y))) -> do
					img <- (!! n) <$> Eff.effBase (readIORef imgs)
					Eff.effBase $ Image.write @IO img x y clr) `mapM_` clrs
				Pipe.yield (fst <$> clrs)
			Pipe.=$= do
				PipeIO.print'
				IO.print @FrameNumber =<< State.getN "foobar"
				FrameNumber n <- State.getN "foobar"
				Eff.effBase $ writeIORef rfn n

	print =<< length <$> readIORef imgs

	imgsd <- readIORef imgs
	gimgs <- toGrayImage `mapM` imgsd

	print hdr

	let hdr' = hdr { Header.headerColorType = Header.ColorTypeGrayscale }

	modifyIORef rfctls (setDisposeOp0 <$>)

	fctls <- readIORef rfctls

	gimgs' <- Image.grayFreeze @IO `mapM` gimgs

	writeApngGray (filePathGray fpo) hdr' (length fctls) 0 $ zipWith FctlImage.fromFctlImageGray fctls gimgs'

	print =<< readIORef rfctls

	print $ length fctls
	print $ length gimgs'

	pure ()

setDisposeOp0 :: Fctl -> Fctl
setDisposeOp0 fctl = fctl { fctlDisposeOp = 0 }

doWhile' :: Monad m => m (Maybe a) -> m a
doWhile' act = act >>= \case
	Nothing -> doWhile' act
	Just r -> pure r


firstFctl :: U.Member Pipe.P m => Eff.E m Body o Fctl
firstFctl = doWhile' $ Pipe.await >>= \case
	BodyFctl fctl -> pure $ Just fctl
	_ -> pure Nothing


writeImage1 :: (U.Member Pipe.P m, U.Base (U.FromFirst IO) m) =>
	IORef [Fctl] ->
	IORef [Image.I RealWorld] -> Fctl -> Eff.E m Body [Rgba Double] (Maybe Fctl)
writeImage1 fctls imgs fctl = do
	Eff.effBase $ putStrLn "writeImage1 begin"
	Eff.effBase $ print fctl
	img <- Eff.effBase $ Image.new @IO
		(fromIntegral $ fctlWidth fctl)
		(fromIntegral $ fctlHeight fctl)
	Eff.effBase $ putStrLn "before modifyIORef"
	Eff.effBase $ modifyIORef fctls (++ [fctl])
	Eff.effBase $ modifyIORef imgs (++ [img])
	Pipe.yield []
	doWhile' do
		d <- Pipe.await
		case d of
			BodyFctl fctl' -> do
				Eff.effBase $ putStrLn "writeImage1 end"
				pure $ Just (Just fctl')
			BodyRgba rgba -> do
				Pipe.yield rgba
				pure Nothing
			BodyNull -> pure Nothing
			BodyFdatEnd -> pure Nothing
			BodyEnd -> do
				Eff.effBase $ putStrLn "writeImage1 end"
				pure $ Just Nothing

whileWithMeta :: Monad m => (meta -> m (Maybe meta)) -> meta -> m ()
whileWithMeta act meta = act meta >>= \case
	Nothing -> pure ()
	Just meta' -> whileWithMeta act meta'

filePathGray :: FilePath -> FilePath
filePathGray fpo = fpbd ++ "-gray" <.> fpex
	where (fpbd, fpex) = splitExtension fpo

data Chunk = Chunk {
	chunkName :: BSF.ByteString,
	chunkBody :: BSF.ByteString }
	deriving Show

toGrayImage :: PrimMonad m => Image.I (PrimState m) -> m (Image.Gray (PrimState m))
toGrayImage i@Image.I { Image.width = w, Image.height = h } = do
	i' <- Image.grayNew w h
	for_ [0 .. h - 1] \y -> for_ [0 .. w - 1] \x ->
		Image.grayWrite i' x y . rgbaToGray @Double =<< Image.read i x y
	pure i'

rgbaToGray :: RealFrac d => Rgba d -> Word8
rgbaToGray (RgbaWord8 r_ g_ b_ _a) = fromIntegral @Int $ (r + g + b) `div` 3
	where
	r = fromIntegral r_
	g = fromIntegral g_
	b = fromIntegral b_
