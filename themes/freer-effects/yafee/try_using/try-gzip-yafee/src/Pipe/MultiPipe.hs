{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.MultiPipe where

import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.OpenUnion qualified as Union
import Data.Maybe

import Data.Char

foo :: (
	Union.Member (Pipe.P Char Int) effs,
	Union.Member (Pipe.P Bool Bool) effs,
	Union.Member IO effs
	) =>
	Eff.E effs ()
foo = do
	mc <- Pipe.await' Int
	maybe (pure ()) (Pipe.yield' Char . ord) mc
	mb <- Pipe.await' Bool
	maybe (pure ()) (Pipe.yield' Bool . not) mb
	when (isJust mc && isJust mb) foo

bar :: forall effs a . forall i -> (
	Union.Member (Pipe.P i a) effs
	) =>
	[a] -> Eff.E effs ()
bar i cs = Pipe.yield' i `mapM_` cs

printAll :: forall i -> forall o -> (
	Union.Member (Pipe.P i o) effs,
	Union.Member IO effs, Show i ) =>
	Eff.E effs ()
printAll i o = do
	Pipe.await' o >>= \case
		Nothing -> pure ()
		Just x -> Eff.eff (print @i x) -- >> printAll i o
	Pipe.await' o >>= \case
		Nothing -> pure ()
		Just x -> Eff.eff (print @i x) -- >> printAll i o
	Pipe.await' o >>= \case
		Nothing -> pure ()
		Just x -> Eff.eff (print @i x) -- >> printAll i o
	Pipe.await' o >>= \case
		Nothing -> pure ()
		Just x -> Eff.eff (print @i x) -- >> printAll i o
	Pipe.await' o >>= \case
		Nothing -> pure ()
		Just x -> Eff.eff (print @i x) -- >> printAll i o
--	Pipe.await' o >>= \case
--		Nothing -> pure ()
--		Just x -> Eff.eff (print @i x) -- >> printAll i o

baz :: forall effs . (
	Union.Member (Pipe.P Bool Bool) effs,
	Union.Member IO effs
	) =>
	Eff.E effs ((), [()])
baz = Pipe.run $ bar @(Pipe.P () Char ': effs) (type ()) "Hello" Pipe.=$=
	foo Pipe.=$=
	printAll @(Pipe.P Int () ': effs) Int (type ())

qux :: forall effs . (
	Union.Member IO effs
	) =>
	Eff.E effs ((), [()])
qux = Pipe.run $ bar @(Pipe.P () Bool ': effs) (type ()) [True, True, False, True, False] Pipe.=$=
	baz @(Pipe.P Bool Bool ': effs) Pipe.=$=
	printAll @(Pipe.P Bool () ': effs) Bool (type ())
