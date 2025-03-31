{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Data.Bits
import Data.Pipe
import Data.Maybe
import Data.List qualified as L
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import System.Environment

import Gzip
import MyMonadNew
import MonadByteString qualified as BS
import BitArray
import MonadBitArray qualified as BA

import Control.Monad.Base
import Control.MonadClasses.State qualified as MC
import Control.MonadClasses.Except qualified as MC

import HuffmanTree
import MonadHuffman

import PipeByteString qualified as PBS

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	putStrLn . take 200 . show =<< runMyPipe ((fixedTable, fixedTable), ExtraBits 0) (bsToBitArray "") (yield cnt =$= do
		BS.print' =<< readHeader
		BS.print' =<< BA.takeBit8 1
		bt <- BA.takeBit8 2
		BS.print' bt
		if bt == 1
		then BA.bits =$= huffmanPipe =$= putDecoded
		else BA.bits =$= (do
			(BS.print' @_ @Word16 . (+ 257) . bitListToNumLE . catMaybes =<< replicateM 5 await)
			(BS.print' @_ @Word8 . (+ 1) . bitListToNumLE . catMaybes =<< replicateM 5 await)
			(BS.print' @_ @Word8 . (+ 4) .  bitListToNumLE . catMaybes =<< replicateM 4 await)
			clcls <- fromList . pairToCodes @Word8 . L.sort . filter ((/= 0) . fst)
				. (`zip` [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12,3, 13, 2, 14, 1, 15])
				<$> replicateM 14 (bitListToNumLE . catMaybes <$> replicateM 3 await)
			BS.print' @_ @(BinTree Int) clcls
			MC.put (clcls, clcls)
			huffmanPipe ) =$= do
				BS.print' =<< await
				MC.put $ ExtraBits 3
				BS.print' =<< await

				BS.print' @_ @Int 9

				BS.print' =<< printWhile 9 \case
					Left x -> 0 <= x && x <= 15
					Right _ -> False
				MC.put $ ExtraBits 7
				BS.print' =<< await

				BS.print' =<< printWhile 32 \case
					Left x -> 0 <= x && x <= 15
					Right _ -> False
				MC.put $ ExtraBits 3
				BS.print' =<< await

				BS.print' =<< printWhile 55 \case
					Left x -> 0 <= x && x <= 15
					Right _ -> False
				MC.put $ ExtraBits 3
				BS.print' =<< await

				BS.print' =<< printWhile 91 \case
					Left x -> 0 <= x && x <= 15
					Right _ -> False
				MC.put $ ExtraBits 7
				BS.print' =<< await

				BS.print' =<< printWhile 256 \case
					Left x -> 0 <= x && x <= 15
					Right _ -> False
				MC.put $ ExtraBits 3
				BS.print' =<< await

				BS.putStrLn' ""
				BS.putStrLn' "== DIST =="

				replicateM_ 18 $ BS.print' =<< await
		)

printWhile :: (
	Show a,
	PipeClass p, MonadBase IO (p a o m),
	Monad m ) => Int ->
	(a -> Bool) -> p a o m Int
printWhile i p = await >>= \case
	Nothing -> pure i
	Just x	| p x -> do
			BS.print' (if i < 256 then Left $ chr i else Right i, x)
			printWhile (i + 1) p
		| otherwise -> i <$ BS.print' x

readHeader :: (
	PipeClass p,
	MC.MonadState BS.ByteString (p BS.ByteString o m),
	MC.MonadError String (p BS.ByteString o m),
	MonadFail (p BS.ByteString o m),
	MonadBase IO (p BS.ByteString o m),
	Monad m ) =>
	p BS.ByteString o m GzipHeader
readHeader = do
		Just ids <- PBS.takeBytes 2
		BS.print' $ ids == ids0
		Just cm <- PBS.popByte
		fs <- maybe (MC.throwError @String "bad flags") pure . readFlags . fromJust =<< PBS.popByte
		Just mt <- PBS.takeWord32
		Just efs <- PBS.popByte
		Just os <- PBS.popByte
		Just fn <- PBS.takeString
		pure GzipHeader {
			gzipHeaderCompressionMethod = cm,
			gzipHeaderFlags = fs,
			gzipHeaderModificationTime = mt,
			gzipExtraFlags = efs,
			gzipOperatingSystem = os,
			gzipFileName = fn }

putDecoded :: (
	PipeClass p,
	MC.MonadState (BinTree Int, BinTree Int) (p (Either Int Word16) o m),
	MC.MonadState ExtraBits (p (Either Int Word16) o m),
	MonadBase IO (p (Either Int Word16) o m),
	Monad m
	) =>
	p (Either Int Word16) o m ()
putDecoded = do
	mi <- await
	BS.print' mi
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> putDecoded
			| 257 <= i && i <= 264 -> MC.put (fixedDstTable, fixedDstTable) >> putDist
			| 265 <= i && i <= 268 -> do
				MC.put $ ExtraBits 1
				putDecoded
			| otherwise -> error "putDecoded: yet"
		Just (Right _) -> do
			MC.put (fixedDstTable, fixedDstTable)
			putDist
		Nothing -> pure ()

putDist :: (
	PipeClass p,
	MC.MonadState (BinTree Int, BinTree Int) (p (Either Int Word16) o m),
	MC.MonadState ExtraBits (p (Either Int Word16) o m),
	MonadBase IO (p (Either Int Word16) o m),
	Monad m
	) =>
	p (Either Int Word16) o m ()
putDist = do
	mi <- await
	BS.print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> MC.put (fixedTable, fixedTable) >> putDecoded
			| 4 <= i && i <= 5 -> do
				MC.put $ ExtraBits 1
				putDist
			| otherwise -> error "putDist: yet"
		Just (Right _) -> do
			MC.put (fixedTable, fixedTable)
			putDecoded
		Nothing -> error "end"

bitListToNumLE :: (Num n, Bits n) => [Bit] -> n
bitListToNumLE = foldr (\b s -> (case b of O -> 0; I -> 1) .|. s `shiftL` 1) 0
