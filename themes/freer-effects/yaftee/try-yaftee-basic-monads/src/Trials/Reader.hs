{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Reader where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.ReaderNew qualified as Reader
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

sampleReaderNew :: (
	U.Member (Reader.R Int) es,
	U.Base IO.I es
	) =>
	Eff.E es i o ()
sampleReaderNew = do
	IO.print =<< Reader.ask @Int
	Reader.local (* (2 :: Int)) do
		IO.print =<< Reader.ask @Int
	IO.print =<< Reader.ask @Int

runSampleReaderNew ::
	Eff.E '[Reader.R Int, IO.I] i o () -> IO ()
runSampleReaderNew = Eff.runM . (`Reader.run` 123)
