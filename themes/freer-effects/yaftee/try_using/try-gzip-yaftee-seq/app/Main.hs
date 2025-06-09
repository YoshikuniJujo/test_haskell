{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Prelude hiding (head)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.Sequence qualified as Seq
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Gzip.Decompress qualified as Gzip

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	let	f = IO.print
	void . Eff.runM
		. Except.run @String . Fail.runExc id . Gzip.run_ @"foobar"
		. Pipe.run
		. (`Except.catch` IO.putStrLn) . void $
			PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$= Gzip.decompress "foobar" f Pipe.=$=
			forever (((Eff.effBase . putChar . chr . fromIntegral) `mapM`) =<< Pipe.await)

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack
