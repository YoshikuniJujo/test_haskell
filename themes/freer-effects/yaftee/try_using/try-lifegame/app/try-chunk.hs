{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand
	qualified as OnDemand
import Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.Png.Chunk
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ho <- openFile fpo WriteMode
	void . Eff.runM . Except.run @String . Fail.run . OnDemand.run @"foo"
		. PipeCrc32.run @"foo" . PipeCrc32.run @"bar" . Pipe.run
		$ hDecode "foo" h Pipe.=$= hEncode "bar" ho
	hClose ho; hClose h
