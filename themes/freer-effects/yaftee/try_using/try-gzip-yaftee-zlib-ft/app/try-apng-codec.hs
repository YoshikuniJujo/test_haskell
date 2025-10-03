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

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer

import Data.IORef

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
	fctls <- newIORef []

	print hdr

	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64
	ibe <- PipeZ.cByteArrayMalloc 64
	obe <- PipeZ.cByteArrayMalloc 64

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
				whileWithMeta (writeImage1 fctls imgs) fctl0
			Pipe.=$= do
				[] <- Pipe.await
				IO.print @FrameNumber =<< State.getN "foobar"
				FrameNumber fn <- State.getN "foobar"

				for_ [0 .. fn - 1] \n -> do

					[] <- Pipe.await
					fctl0 <- (!! n) <$> Eff.effBase (readIORef fctls)
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

	fn <- readIORef rfn

	for_ (zip [0 .. fn - 1] (filePath fpo <$> [0 :: Int ..])) \(n, fpp) ->
		writePng fpp hdr fctls imgs ibe obe n

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

writePng ::
	FilePath -> Header.Header ->
	IORef [Fctl] -> IORef [Image.I RealWorld] ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> Int -> IO ()
writePng fpp hdr fctls imgs ibe obe n = do
	ho <- openFile fpp WriteMode
	fctl <- (!! n) <$> readIORef fctls
	img <- (!! n) <$> readIORef imgs

	hWritePng ho hdr fctl img ibe obe

	hClose ho

hWritePng :: (Monad m, U.Base (U.FromFirst IO) '[U.FromFirst m]) =>
	Handle -> Header.Header -> Fctl -> Image.I RealWorld ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld -> m ()
hWritePng ho hdr fctl img ibe obe =
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"barbaz" @BSF.ByteString . PipeZ.run @"barbaz"
		. Pipe.run $ hWritePngPipe ho hdr fctl img ibe obe

hWritePngPipe :: (
	U.Member Pipe.P es,
	U.Member (State.Named "barbaz" (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named "barbaz" (Maybe PipeZ.ByteString)) es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Base IO.I es ) =>
	Handle -> Header.Header -> Fctl -> Image.I RealWorld ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es i o ()
hWritePngPipe ho hdr fctl img ibe obe = (`Except.catch` IO.putStrLn)
	. void $ fromImage @Double IO img (fctlPoss' hdr fctl)
		Pipe.=$= PipeT.convert BodyRgba
		Pipe.=$= Encode.encodeRaw "barbaz" IO
			hdr {
				Header.headerWidth = fctlWidth fctl,
				Header.headerHeight = fctlHeight fctl }
			Nothing ibe obe
		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho

filePath :: FilePath -> Int -> FilePath
filePath fpo n = fpbd ++ "-" ++ showN 2 n <.> fpex
	where (fpbd, fpex) = splitExtension fpo

showN :: Show n => Int -> n -> String
showN ln n = replicate (ln - length s) '0' ++ s where s = show n
