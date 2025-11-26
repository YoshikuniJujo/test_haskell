{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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

import Control.Monad.Yaftee.State qualified as St
import Data.TypeLevel.List
import Data.Word.Crc32 qualified as Crc32
import Data.HigherFunctor qualified as F
import Control.HigherOpenUnion qualified as U

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ho <- openFile fpo WriteMode
	void . Eff.runM . encodeRun_ @"foo" . PipeCrc32.run @"bar"
		. Except.run @String . Fail.run . Pipe.run
		$ hDecode "foo" h 32 50 Pipe.=$= hEncode "bar" ho
	hClose ho; hClose h

encodeRun_ :: forall nm es i o r .
	F.Loose (U.U es) =>
	Eff.E (St.Named nm Crc32.C ': OnDemand.States nm `Append` es) i o r ->
	Eff.E es i o ()
encodeRun_ = void . OnDemand.run @nm . PipeCrc32.run @nm
