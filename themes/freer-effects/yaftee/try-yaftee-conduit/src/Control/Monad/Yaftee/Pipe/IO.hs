{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.IO where

import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (print)
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bool
import System.IO

print :: forall es i o r .
	(Show i, U.Member Pipe.P es, U.Base IO.I es) => Eff.E es i o r
print = fix \go -> Pipe.await >>= (>> go) . IO.print

print' :: forall es i o .
	(Show i, U.Member Pipe.P es, U.Base IO.I es) => Eff.E es i o ()
print' = fix \go ->
	Pipe.isMore >>= bool (pure ()) (Pipe.await >>= (>> go) . IO.print)

hPutStorable :: forall es a o r .
	(Storable a, U.Member Pipe.P es, U.Base IO.I es) =>
	Handle -> Ptr a -> Eff.E es a o r
hPutStorable h p = fix \go -> Pipe.await >>= \x ->
	Eff.effBase (poke p x >> hPutBuf h p (sizeOf x)) >> go

hGetStorable :: forall es i a . (
	Storable a, U.Member Pipe.P es,
	U.Member (Except.E String) es, U.Base IO.I es ) =>
	Handle -> Ptr a -> Eff.E es i a ()
hGetStorable h p = fix \go -> do
	rsz <- Eff.effBase $ hGetBuf h p sz
	case rsz of
		0 -> pure ()
		_	| rsz < sz -> Except.throw "hGetStorable: Not enough input"
			| rsz == sz -> do
				Pipe.yield =<< Eff.effBase (peek p)
				go
			| otherwise -> Except.throw "never occur"
	where sz = sizeOf (undefined :: a)
