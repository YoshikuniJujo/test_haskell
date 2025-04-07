{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryPrimMonad where

import Control.Monad.Primitive
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Vector.Mutable qualified as Vector

foo :: PrimMonad m => m Int
foo = do
	v <- Vector.new 10
	Vector.write v 3 123
	Vector.read v 3

send :: Union.Member (Pipe.P () Int) effs => [Int] -> Eff.E effs ()
send xs = Pipe.yield @() `mapM_` xs

put :: forall m effs . (
	PrimMonad m,
	Union.Member m effs,
	Union.Member (Pipe.P Int ()) effs ) =>
	Vector.MVector (PrimState m) (Maybe Int) -> Eff.E effs ()
put v = do
	x <- Pipe.await @Int @()
	Eff.eff $ Vector.write @m v 3 x

{-
bar :: forall m effs . (
	PrimMonad m,
	Union.Member m effs ) =>
	Eff.E (Pipe.P Int () ': effs) ()
bar = do
	v <- Eff.eff $ Vector.new @m 10
	send [3, 5, 8] Pipe.=$= put @m v
	-}
