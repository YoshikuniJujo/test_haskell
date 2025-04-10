{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.ByteString.IO
import Pipe.ByteString.OnDemand

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(print =<<) . Eff.runM . (Pipe.run @() @() @'[IO]) $
		fromHandle @[Pipe.P () BS.ByteString, IO] (type ()) h Pipe.=$=
		(Eff.eff . print =<< Pipe.await @BS.ByteString @())
