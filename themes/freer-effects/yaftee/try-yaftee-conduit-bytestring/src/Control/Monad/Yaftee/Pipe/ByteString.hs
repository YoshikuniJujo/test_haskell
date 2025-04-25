{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as Union
import Data.Bool
import Data.ByteString qualified as BS
import System.IO

hGet :: Union.Base IO.I es =>
	Int -> Handle -> Eff.E (Pipe.P i BS.ByteString ': es) ()
hGet bfsz h = fix \go -> Eff.effBase (not <$> hIsEOF h) >>=
	bool (pure ()) (Eff.effBase (BS.hGetSome h bfsz) >>= Pipe.yield >> go)

putStr :: Union.Base IO.I es => Eff.E (Pipe.P BS.ByteString o ': es) ()
putStr = fix \go -> Pipe.await >>= Eff.effBase . BS.putStr >> go
