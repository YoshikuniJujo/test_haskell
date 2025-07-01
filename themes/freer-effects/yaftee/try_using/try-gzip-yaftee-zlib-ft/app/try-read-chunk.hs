{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSFB
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
		. Steps.chunkRun_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Steps.chunk "foobar" Pipe.=$=
				forever do
					bd <- Pipe.await
					Steps.Chunk nm <-
						State.getN @Steps.Chunk "foobar"
					Pipe.yield $ Chunk {
						chunkName = nm,
						chunkBody = bd }
			Pipe.=$= PipeIO.print

data Chunk = Chunk {
	chunkName :: BSF.ByteString,
	chunkBody :: BSF.ByteString }
	deriving Show

chunkToByteString :: Chunk -> BSF.ByteString
chunkToByteString Chunk { chunkName = nm, chunkBody = bd } =
	BSFB.fromBits' ln
	where
	ln = fromIntegral @_ @Word32 $ BSF.length bd
