{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Zlib.Decompress qualified as Zlib
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import System.IO
import System.Environment

import Pipe.Huffman qualified as Huffman

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String . Fail.run . Zlib.runNew_ @"foobar"
		. (`State.run` Huffman.IsLiteral (const False :: Int -> Bool))
		. (`State.run` Huffman.PhaseOthers)
		. Pipe.run $ PipeBS.hGet 64 h Pipe.=$= ((void $
			OnDemand.onDemand "foobar" Pipe.=$=
			Zlib.decompressNew "foobar" 100 Pipe.=$= PipeBS.putStr ) `Except.catch` IO.putStrLn)
