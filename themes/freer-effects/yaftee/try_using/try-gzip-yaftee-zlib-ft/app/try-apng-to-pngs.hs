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
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
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
				whileWithMeta (writeImage1 fctls imgs) fctl0
			Pipe.=$= do
				[] <- Pipe.await
				fctl0 : _ <- Eff.effBase $ readIORef fctls
				pipeZip $ (0 ,) <$> (fctlPoss hdr fctl0)
				[] <- Pipe.await
				_ : fctl1 : _ <- Eff.effBase $ readIORef fctls
				pipeZip $ (1 ,) <$> (fctlPoss hdr fctl1)
			Pipe.=$= forever do
				clrs <- Pipe.await
				(\(clr, (n, (x, y))) -> do
					img <- (!! n) <$> Eff.effBase (readIORef imgs)
					Eff.effBase $ Image.write @IO img x y clr) `mapM_` clrs
				Pipe.yield (fst <$> clrs)
			Pipe.=$= PipeIO.print'

	print =<< length <$> readIORef imgs

	ho <- openFile fpo WriteMode
	img <- (!! 0) <$> readIORef imgs
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"barbaz" @BSF.ByteString
		. PipeZ.run @"barbaz"
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ fromImage @Double IO img (Header.headerToPoss' hdr)
			Pipe.=$= Encode.encodeRgba "barbaz" IO hdr ibe obe
			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeBS.hPutStr ho
	hClose ho

{-
	let	(fpbd, fpex) = splitExtension fpo
		fpo01 = fpbd ++ "-01" <.> fpex

	ho' <- openFile fpo01 WriteMode
	img1 <- (!! 1) <$> readIORef imgs
	fctl1 <- (!! 1) <$> readIORef fctls
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"barbaz" @BSF.ByteString
		. PipeZ.run @"barbaz"
		. Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ fromImage @Double IO img1 (fctlPoss' hdr fctl1)
			Pipe.=$= Encode.encodeRgba "barbaz" IO hdr ibe obe
			Pipe.=$= PipeT.convert BSF.toStrict
			Pipe.=$= PipeBS.hPutStr ho'
	hClose ho'
-}

doWhile :: Monad m => m Bool -> m ()
doWhile act = do
	b <- act
	when b $ doWhile act

doWhile' :: Monad m => m (Maybe a) -> m a
doWhile' act = act >>= \case
	Nothing -> doWhile' act
	Just r -> pure r


writeImages :: (U.Member Pipe.P m, U.Base (U.FromFirst IO) m) =>
	IORef [Image.I RealWorld] -> Eff.E m Body [Rgba Double] ()
writeImages imgs = do
	doWhile do
		d <- Pipe.await
		case d of
			BodyFctl fctl -> do
				img <- Eff.effBase $ Image.new @IO
					(fromIntegral $ fctlWidth fctl)
					(fromIntegral $ fctlHeight fctl)
				Eff.effBase $ writeIORef imgs [img]
				pure False
			_ -> pure True
	doWhile do
		d <- Pipe.await
		case d of
			BodyFctl _ -> pure False
			BodyRgba rgba -> do
				Pipe.yield rgba
				pure True
			BodyNull -> pure True

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
--	Eff.effBase $ modifyIORef imgs (img :)
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
			BodyEnd -> do
				Eff.effBase $ putStrLn "writeImage1 end"
				pure $ Just Nothing

whileWithMeta :: Monad m => (meta -> m (Maybe meta)) -> meta -> m ()
whileWithMeta act meta = act meta >>= \case
	Nothing -> pure ()
	Just meta' -> whileWithMeta act meta'
