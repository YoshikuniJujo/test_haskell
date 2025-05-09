{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.List where

import Control.Arrow
import Control.Monad.Fix
import Data.Maybe
import Data.Bool
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as Union

from xs = Pipe.yield `traverse` xs

to :: forall effs i o o' r .
	(Union.HFunctor' (Union.U effs), Union.Member Fail.F effs) =>
	Eff.E (Pipe.P ': effs) i o r -> Eff.E effs i o' [o]
to p = fromJust <$> Pipe.run do
	(_, HFreer.Pure r) <- p Pipe.=$= fix \go -> do
		Pipe.isMore >>= bool (pure []) ((:) <$> Pipe.await <*> go)
	pure r

bundle' :: Union.Member Pipe.P es => Int -> Eff.E es a [a] ()
bundle' n = fix \go -> do
	(f, xs) <- replicateAwait n
	Pipe.yield xs
	bool go (pure ()) f

replicateAwait :: Union.Member Pipe.P es => Int -> Eff.E es a o (Bool, [a])
replicateAwait = \case
	0 -> pure (False, [])
	n -> Pipe.isMore >>= bool
		(pure (True, []))
		(Pipe.await >>= \x -> ((x :) `second`) <$> replicateAwait (n - 1))
