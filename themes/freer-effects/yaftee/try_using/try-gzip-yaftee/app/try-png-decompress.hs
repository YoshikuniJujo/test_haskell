{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where


import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Png.DecodeNew

main :: IO ()
main = do
	fp : _ <- getArgs
	let	processHeader = IO.print
	h <- openFile fp ReadMode
	void . Eff.runM

		. pngRun @"chunk" @"deflate"

		. Except.run @String . Fail.runExc id . Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			(void (png "chunk" "deflate" processHeader) `Except.catch` IO.print @String) Pipe.=$= do
			PipeIO.print'
