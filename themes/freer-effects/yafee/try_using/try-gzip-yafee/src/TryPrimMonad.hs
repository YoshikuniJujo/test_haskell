{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryPrimMonad where

import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Maybe
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
	Union.Base m effs,
	Union.Member (Pipe.P Int ()) effs ) =>
	Vector.MVector (PrimState m) (Maybe Int) -> Eff.E effs ()
put v = do
	x <- Pipe.await @Int @()
	Eff.effBase $ Vector.write @m v 3 x

bar :: forall m effs . (
	PrimMonad m,
	Union.Base m (Pipe.P Int () ': effs),
	Union.Base m (Pipe.P () () ': effs) ) =>
	Eff.E (Pipe.P () () ': effs) (Vector.MVector (PrimState m) (Maybe Int))
bar = do
	v <- Eff.effBase $ Vector.new @m 10
	send @(Pipe.P () Int ': effs)  [123, 5, 8] Pipe.=$=
		put @m @(Pipe.P Int () ': effs) v
	pure v

barRunned' :: forall m . PrimMonad m => m (Maybe (Vector.MVector (PrimState m) (Maybe Int)))
barRunned' = Eff.runM $ Pipe.run (bar @m :: Eff.E '[(Pipe.P () ()), m] (Vector.MVector (PrimState m) (Maybe Int)))

tryIO :: IO (Maybe Int)
tryIO = barRunned' >>= (`Vector.read` 3) . fromJust

tryST :: Maybe Int
tryST = runST $ barRunned' >>= (`Vector.read` 3) . fromJust
