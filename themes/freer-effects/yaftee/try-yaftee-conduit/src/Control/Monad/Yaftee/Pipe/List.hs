{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.List (
	from, to, bundle, bundle' ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.HigherFreer qualified as F
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.HigherFunctor qualified as Fn
import Data.Maybe
import Data.Bool

from :: forall f es i a .
	(Foldable f, U.Member Pipe.P es) => f a -> Eff.E es i a ()
from xs = Pipe.yield `traverse_` xs

to :: forall es i o o' r .
	Fn.Tight (U.U es) => Eff.E (Pipe.P ': es) i o r -> Eff.E es i o' [o]
to p = (fromJust <$>) . Pipe.run $ fromPure . snd <$> p Pipe.=$= fix \go ->
	Pipe.isMore >>= bool (pure []) ((:) <$> Pipe.await <*> go)

fromPure :: F.H h i o a -> a
fromPure = \case F.Pure x -> x; _ -> error "not Pure"

bundle :: U.Member Pipe.P es => Int -> Eff.E es a [a] r
bundle n = fix \go -> (Pipe.yield =<< replicateM n Pipe.await) >> go

bundle' :: U.Member Pipe.P es => Int -> Eff.E es a [a] ()
bundle' n = fix \go -> do
	(f, xs) <- replicateAwait n
	Pipe.yield xs
	bool go (pure ()) f

replicateAwait :: U.Member Pipe.P es => Int -> Eff.E es a o (Bool, [a])
replicateAwait = \case
	0 -> pure (False, [])
	n -> Pipe.isMore >>= bool
		(pure (True, []))
		(Pipe.await >>= \x -> ((x :) `second`) <$> replicateAwait (n - 1))
