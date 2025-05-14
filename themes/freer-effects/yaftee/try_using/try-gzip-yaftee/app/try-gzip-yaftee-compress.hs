{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.Bits qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Bit qualified as Bit
import System.IO
import System.Environment

import Pipe.RunLength.Compress qualified as RunLength
import Data.Gzip.GzipHeader
import Data.Gzip.Block

main :: IO ()
main = do
	ifp : ofp : _ <- getArgs
	withFile ifp ReadMode \hr -> withFile ofp WriteMode \ho ->
		Eff.runM
			. (`State.run` Bit.empty)
			. (`State.run` ("" :: BS.ByteString))
			. (`State.run` FileLength 0)
			. PipeCrc.runCrc32 . RunLength.run . PipeL.to
			$ PipeBS.hGet 64 hr Pipe.=$=
				PipeCrc.crc32' Pipe.=$=
				lengthPipe Pipe.=$=
				RunLength.compressRL Pipe.=$=
--				PipeIO.print
--				{-
				PipeL.bundle' 500 Pipe.=$=
				PipeT.convert'' runLengthsToBits [] Pipe.=$= do
					Pipe.yield $ encodeGzipHeader sampleGzipHeader
					PipeB.toByteString'
					PipeCrc.compCrc32
					Pipe.yield . PipeCrc.crc32ToByteString =<< State.getN PipeBS.Pkg
					Pipe.yield . numToBs' 4 . unFileLength =<< State.get
				Pipe.=$= PipeBS.hPutStr' ho
--				-}
	pure ()

lengthPipe :: (U.Member Pipe.P es, U.Member (State.S FileLength) es) =>
	Eff.E es BS.ByteString BS.ByteString ()
lengthPipe = (Pipe.isMore >>=) . bool (pure ()) $ Pipe.await >>= \bs -> do
	State.modify $ FileLength . (BS.length bs +) . unFileLength
	Pipe.yield bs >> lengthPipe

newtype FileLength = FileLength { unFileLength :: Int } deriving Show

numToBs' ln _ | ln < 1 = ""
numToBs' ln n = fromIntegral (n .&. 0xff) `BS.cons` numToBs' (ln - 1) (n `shiftR` 8)
