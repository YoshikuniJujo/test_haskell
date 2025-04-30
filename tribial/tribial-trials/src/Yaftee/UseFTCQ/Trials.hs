{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.UseFTCQ.Trials where

import Data.Functor.Identity
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.ReaderI qualified as Reader
import Yaftee.UseFTCQ.WriterO qualified as Writer
import Yaftee.OpenUnion qualified as Union

sampleReaderWriter :: (
	Show x,
	Union.Member Reader.R effs,
	Union.Member Writer.W effs,
	Union.Base (Union.FromFirst IO) effs
	) =>
	Eff.E effs x String ()
sampleReaderWriter = do
	x <- Reader.ask
	Eff.effBase $ print x
	Writer.tell $ show x

runSampleReaderWriter1 :: IO (String, Identity ())
runSampleReaderWriter1 = Eff.runM . Writer.run $ sampleReaderWriter `Reader.run` 54321

runSampleReaderWriter2 :: IO (Identity (String, ()))
runSampleReaderWriter2 = Eff.runM . (`Reader.run` 54321) $ Writer.run sampleReaderWriter
