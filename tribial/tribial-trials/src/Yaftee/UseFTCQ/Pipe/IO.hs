{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.IO where

import Prelude hiding (print)
import Prelude qualified as P

import Foreign.Ptr
import Foreign.Storable
import Control.Monad.Fix
import System.IO
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.UseFTCQ.IO qualified as IO
import Yaftee.OpenUnion qualified as Union

import System.IO qualified as IO

print :: (Show i, Union.Member Pipe.P effs, Union.Base IO.I effs) =>
	Eff.E effs i o r
print = fix \go -> Pipe.await >>= (>> go) . Eff.effBase . P.print

print' :: (Show i, Union.Member Pipe.P effs, Union.Base IO.I effs) =>
	Eff.E effs i o ()
print' = fix \go -> do
	b <- Pipe.isMore
	if b
	then Pipe.await >>= (>> go) . Eff.effBase . P.print
	else pure ()

hPutStorable :: (
	Storable a,
	Union.Member Pipe.P effs, Union.Base IO.I effs ) =>
	Handle -> Ptr a -> Eff.E effs a o ()
hPutStorable h p = fix \go -> Pipe.await >>= \x ->
	Eff.effBase (poke p x) >> Eff.effBase (IO.hPutBuf h p $ sizeOf x) >> go

hGetStorable :: forall i a effs . (
	Storable a,
	Union.Member Pipe.P effs,
	Union.Member (Except.E String) effs,
	Union.Base IO.I effs ) =>
	Handle -> Ptr a -> Eff.E effs i a ()
hGetStorable h p = fix \go -> do
	rsz <- Eff.effBase $ IO.hGetBuf h p sz
	case rsz of
		0 -> pure ()
		_	| rsz < sz ->
				Except.throw "hGetStorable: Not enough input"
			| rsz == sz -> do
				Pipe.yield =<< Eff.effBase (peek p)
				go
			| otherwise -> Except.throw "never occur"
	where sz = sizeOf (undefined :: a)
