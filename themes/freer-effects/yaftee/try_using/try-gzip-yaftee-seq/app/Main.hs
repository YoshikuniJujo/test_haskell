{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$=
			PipeIO.print

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

seqToBs :: Seq.Seq Word8 -> BS.ByteString
seqToBs = BS.pack . toList
