{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import System.IO

main :: IO ()
main = do
	h <- openFile "sample/foo.txt.gz" ReadMode
	print =<< Eff.runM (PipeL.to $ PipeB.hGet' 64 h)
