{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Pipe.ByteString where

import Control.Monad.Fix
import Data.ByteString qualified as BS
import Data.Bool
import System.IO

import Yaftee.Eff qualified as Eff
import Yaftee.NewPipe qualified as Pipe
import Yaftee.Pipe.IO qualified as IO
import Yaftee.OpenUnion qualified as Union

hGet :: (Union.Member Pipe.Yield es, Union.Base IO.I es) =>
	Int -> Handle -> Eff.E es i BS.ByteString ()
hGet bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>=
	bool (pure ()) (Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield >> go)
