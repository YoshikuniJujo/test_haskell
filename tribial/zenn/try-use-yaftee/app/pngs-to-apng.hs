{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as P
import Control.Monad.Yaftee.Pipe.Tools qualified as PT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as C
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Ex
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.List qualified as L
import Data.Ratio
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.ByteString qualified as BS
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png qualified as Png
import Data.Apng qualified as Apng
import System.IO
import System.Environment
import System.Directory
import System.FilePath
import Lifegame.Png.Chunk.Encode qualified as CEn
import Lifegame.Tools

body :: forall nm -> (
	U.Member P.P es, U.Member (State.Named nm BSF.ByteString) es,
	U.Member (Ex.E String) es ) => Eff.E es C.C o BSF.ByteString
body nm = P.await >>= \case
	C.Body bd -> State.modifyN nm (<> bd) >> body nm
	C.End -> State.getN nm <* State.putN @BSF.ByteString nm ""
	_ -> Ex.throw @String "body: not body"

header :: Word32 -> Word32 -> Png.Header
header w h = Png.Header {
	Png.width = w, Png.height = h, Png.bitDepth = 1,
	Png.colorType = Png.ColorTypeGrayscale,
	Png.compressionMethod = Png.CompressionMethodDeflate,
	Png.filterMethod = Png.FilterMethodDefaultFilter,
	Png.interlaceMethod = Png.InterlaceMethodNon }

actl :: Word32 -> Apng.Actl
actl fn = Apng.Actl { Apng.actlFrames = fn, Apng.actlPlays = 0 }

fctl :: Word32 -> Word32 -> Ratio Word16 -> Apng.Fctl
fctl w h d = Apng.Fctl {
	Apng.fctlWidth = w, Apng.fctlHeight = h,
	Apng.fctlXOffset = 0, Apng.fctlYOffset = 0, Apng.fctlDelay = d,
	Apng.fctlDisposeOp = Apng.DisposeOpNone,
	Apng.fctlBlendOp = Apng.BlendOpSource }

fdat :: Word32 -> BSF.ByteString -> CEn.C
fdat sn bd = CEn.C "fdAT" $ BSF.fromBitsBE' sn <> bd

chunks :: (
	U.Member P.P es,
	U.Member (State.Named "dec" BSF.ByteString) es,
	U.Member (State.Named "dec" [Ratio Word16]) es,
	U.Member (Ex.E String) es, U.Member Fail.F es ) =>
	Word32 -> Word32 -> Int -> Eff.E es C.C (Word32 -> (CEn.C, Word32)) ()
chunks w h n = do
	P.yield (CEn.C "IHDR" . BSF.fromStrict . Png.encode $ header w h ,)
	P.yield (CEn.C "acTL" . Apng.encodeActl . actl $ fromIntegral n ,)
	C.Begin _ "IHDR" <- P.await; _ <- body "dec"
	Just d <- pop "dec"
	P.yield \s -> (CEn.C "fcTL" (Apng.encodeFctl s $ fctl w h d), s + 1)
	doWhile_ do
		C.Begin _ cnm <- P.await
		case cnm of
			"IDAT" -> True <$
				(P.yield . (,) . CEn.C "IDAT" =<< body "dec")
			"IEND" -> do C.End <- P.await; pure False
			_ -> pure True
	doWhile_ $ P.awaitMaybe >>= \case
		Nothing -> pure False
		Just C.EndOfTheWorld -> pure True
		Just (C.Begin _ "IHDR") -> do	
			_ <- body "dec"
			Just d' <- pop "dec"
			P.yield \s -> (
				CEn.C "fcTL" (Apng.encodeFctl s $ fctl w h d'),
				s + 1 )
			doWhile_ do
				C.Begin _ cnm <- P.await
				case cnm of
					"IDAT" -> True <$ do
						bd <- body "dec"
						P.yield \s -> (fdat s bd, s + 1)
					"IEND" ->
						do C.End <- P.await; pure False
					_ -> pure True
			pure True
		_ -> Ex.throw @String "bad"
	P.yield (CEn.C "IEND" "" ,)

main :: IO ()
main = do
	d : dl_ : dle_ : fpo : _ <- getArgs

	fps@(fp : _) <- ((d </>) <$>) . L.sort
		. filter (not . ("." `L.isSuffixOf`)) <$> getDirectoryContents d
	let	n = length fps
		dl = read dl_ :: Ratio Word16
		dle = read dle_ :: Ratio Word16

	-- get image width and height from IHDR chunk of first PNG file

	hh <- openFile fp ReadMode
	((), Just (w, h)) <- Eff.runM
		. (flip (State.run @_ @(Maybe (Word32, Word32))) Nothing)
		. C.decodeRun_ @"hdr"
		. flip (State.runN @"hdr") ("" :: BSF.ByteString)
		. Ex.run @String . Fail.run . P.run . (`Ex.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 hh P.=$= PT.convert BSF.fromStrict
			P.=$= C.decode "hdr" 50 P.=$= do
				C.Begin 13 "IHDR" <- P.await
				State.put =<< hdrToSize <$> body "hdr"
	hClose hh

	-- create APNG data and write to file

	ho <- openFile fpo WriteMode
	void . Eff.runM
		. C.decodeRun_ @"dec" . CEn.encodeRun_ @"enc"
		. flip (State.runN @"dec") ("" :: BSF.ByteString)
		. flip (State.runN @"dec") (replicate (n - 1) dl ++ [dle])
		. Ex.run @String . Fail.run . P.run
		. (`Ex.catch` IO.putStrLn) . (`Fail.catch` IO.putStrLn)
		. void $ getFile 32 `mapM_` fps P.=$= PT.convert BSF.fromStrict
			P.=$= replicateM_ n (C.decode "dec" 50)
			P.=$= chunks w h n P.=$= CEn.encode' "enc" 0
			P.=$= PT.convert BSF.toStrict P.=$= PipeBS.hPutStr ho
	hClose ho

hdrToSize :: BSF.ByteString -> Maybe (Word32, Word32)
hdrToSize hdr = do
	(wdt, hdr') <- first Word8.toBitsBE <$> BSF.splitAt' 4 hdr
	(hgt, _) <- first Word8.toBitsBE <$> BSF.splitAt' 4 hdr'
	pure (wdt, hgt)

getFile :: (U.Member P.P es, U.Base IO.I es) =>
	Int -> FilePath -> Eff.E es i BS.ByteString ()
getFile n fp = (>>) <$> PipeBS.hGet n <*> Eff.effBase . hClose
	=<< Eff.effBase (openFile fp ReadMode)
