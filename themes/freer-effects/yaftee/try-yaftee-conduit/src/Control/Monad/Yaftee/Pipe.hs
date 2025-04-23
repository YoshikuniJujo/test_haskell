{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
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
	Eff.E (P i o ': effs) (Eff.E (P i a ': effs) r, r')
p =$= HFreer.Pure r = HFreer.Pure (p, r)
p@(u HFreer.:>>= q) =$= p'@(v HFreer.:>>= r) =
	case (Union.decomp u, Union.decomp v) of
		(_, Left v') ->
			Union.weaken (HFunctor.map (p =$=) (p ,) v') HFreer.:>>=
			Q.singleton \(p'', x) -> p'' =$= (r `HFreer.app` x)
		(_, Right (Union.FromFirst (Yield o))) ->
			Union.inj (Yield @i o) HFreer.:>>=
			Q.singleton ((p =$=) . (r `HFreer.app`))
		(Right (Union.FromFirst Await), _) ->
			Union.inj (Await @_ @o) HFreer.:>>=
			Q.singleton ((=$= p') . (q `HFreer.app`))
		(Right (Union.FromFirst (Yield o)), Right (Union.FromFirst Await)) ->
			(q `HFreer.app` ()) =$= (r `HFreer.app` Just o)
		(Left u', Right (Union.FromFirst Await)) ->
			Union.weaken (HFunctor.map (=@= p') (, p') u') HFreer.:>>=
			Q.singleton \(x, p'') -> (q `HFreer.app` x) =$= p''
p@(HFreer.Pure _) =$= (v HFreer.:>>= r) = case Union.decomp v of
	Left v' -> Union.weaken  (HFunctor.map (p =$=) (p ,) v') HFreer.:>>=
		Q.singleton \(p'', x) -> p'' =$= (r `HFreer.app` x)
	Right (Union.FromFirst Await) -> p =$= (r `HFreer.app` Nothing)
	Right (Union.FromFirst (Yield o)) -> Union.inj (Yield @i o) HFreer.:>>=
		Q.singleton ((p =$=) . (r `HFreer.app`))

(=@=) :: forall i a o effs r r' . HFunctor.H (Union.U effs) =>
	Eff.E (P i a ': effs) r -> Eff.E (P a o ': effs) r' ->
	Eff.E (P i o ': effs) (r, Eff.E (P a o ': effs) r')
HFreer.Pure r =@= p' = HFreer.Pure (r, p')
p@(u HFreer.:>>= q) =@= p'@(v HFreer.:>>= r) =
	case (Union.decomp u, Union.decomp v) of
		(Left u', _) ->
			Union.weaken (HFunctor.map (=@= p') (, p') u') HFreer.:>>=
			Q.singleton \(x, p'') -> (q `HFreer.app` x) =@= p''
		(_, Right (Union.FromFirst (Yield o))) ->
			Union.inj (Yield @i o) HFreer.:>>=
			Q.singleton ((p =@=) . (r `HFreer.app`))
		(Right (Union.FromFirst Await), _) ->
			Union.inj (Await @_ @o) HFreer.:>>=
			Q.singleton ((=@= p') . (q `HFreer.app`))
		(Right (Union.FromFirst (Yield o)), Right (Union.FromFirst Await)) ->
			(q `HFreer.app` ()) =@= (r `HFreer.app` Just o)
		(Right (Union.FromFirst (Yield _o)), Left v') ->
			Union.weaken (HFunctor.map (p =$=) (p ,) v') HFreer.:>>=
			Q.singleton \(p'', x) -> ((p'' =@=) . (r `HFreer.app`)) x
(u HFreer.:>>= q) =@= p'@(HFreer.Pure _) = case Union.decomp u of
	Left u' -> Union.weaken (HFunctor.map (=@= p') (, p') u') HFreer.:>>=
		Q.singleton \(x, p'') -> ((=@= p'') . (q `HFreer.app`)) x
	Right (Union.FromFirst Await) -> Union.inj (Await @_ @o) HFreer.:>>=
		Q.singleton ((=@= p') . (q `HFreer.app`))
	Right (Union.FromFirst (Yield _o)) -> q `HFreer.app` () =@= p'
