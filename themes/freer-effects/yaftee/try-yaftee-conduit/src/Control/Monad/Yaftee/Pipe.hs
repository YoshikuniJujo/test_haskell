{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

data P_ i o r where
	Await :: P_ i o (Maybe i)
	Yield :: forall i o . o -> P_ i o ()

type P i o = Union.FromFirst (P_ i o)

(=$=) :: forall i a o effs r r' . HFunctor.H (Union.U effs) =>
	Eff.E (P i a ': effs) r -> Eff.E (P a o ': effs) r' ->
	Eff.E (P i o ': effs) r'
_ =$= HFreer.Pure r = HFreer.Pure r
p@(u HFreer.:>>= q) =$= p'@(v HFreer.:>>= r) =
	case (Union.decomp u, Union.decomp v) of
		(_, Left v') ->
			Union.weaken (HFunctor.map (p =$=) id v') HFreer.:>>=
			Q.singleton ((p =$=) . (r `HFreer.app`))
		(_, Right (Union.FromFirst (Yield o))) ->
			Union.inj (Yield @i o) HFreer.:>>=
			Q.singleton ((p =$=) . (r `HFreer.app`))
		(Right (Union.FromFirst Await), _) ->
			Union.inj (Await @_ @o) HFreer.:>>=
			Q.singleton ((=$= p') . (q `HFreer.app`))
		(Right (Union.FromFirst (Yield o)), Right (Union.FromFirst Await)) ->
			(q `HFreer.app` ()) =$= (r `HFreer.app` Just o)
		(Left u', Right (Union.FromFirst Await)) ->
--			Union.weaken (HFunctor.map (=$= p') (const undefined) u') HFreer.:>>=
			Union.weaken (HFunctor.map (const undefined) (const undefined) u') HFreer.:>>=
			Q.singleton ((=$= p') . (q `HFreer.app`))
