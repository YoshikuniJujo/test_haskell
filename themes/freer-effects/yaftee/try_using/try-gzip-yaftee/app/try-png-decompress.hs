{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Bits
import Data.Bits.ToolsYj
import Data.ByteString qualified as BS
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Crc.runCrc32 @"foobar"
		. OnDemand.run_ @"foobar"
		. Except.run @String
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$= OnDemand.onDemand "foobar" Pipe.=$=
			PipeT.checkRight Pipe.=$= Crc.crc32 "foobar" Pipe.=$= do
				State.putN "foobar" $ OnDemand.RequestBytes 8
				Pipe.await
				doWhile_ $ chunk1 "foobar" 10
			Pipe.=$= PipeIO.print

chunk1 :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BS.ByteString (Either ChunkTag BS.ByteString) Bool
chunk1 nm m = do
	State.putN nm $ OnDemand.RequestBytes 4
	n <- be <$> Pipe.await

	Crc.resetCrc32 nm
	State.putN nm (OnDemand.RequestBytes 4)
	cn <- Pipe.await
	Pipe.yield . Left $ ChunkBegin cn

	for_ (split m n) \n' -> do
		State.putN nm $ OnDemand.RequestBytes n'
		Pipe.yield =<< Right <$> Pipe.await

	Crc.compCrc32 nm

	crc1 <- State.getN nm

	State.putN nm $ OnDemand.RequestBytes 4
	crc0 <- Crc.byteStringToCrc32BE <$> Pipe.await

	when (Just crc1 /= crc0) $ Except.throw "chunk1: CRC32 error"

	Pipe.yield . Left $ ChunkEnd cn

	pure $ cn /= "IEND"

data ChunkTag = ChunkBegin BS.ByteString | ChunkEnd BS.ByteString deriving Show

be :: Bits n => BS.ByteString -> n
be = BS.foldl (\s b -> s `shiftL` 8 .|. bitsToBits 8 b) zeroBits

split :: Int -> Int -> [Int]
split n 0 = []
split n m
	| n < m = n : split n (m - n)
	| otherwise = [m]
