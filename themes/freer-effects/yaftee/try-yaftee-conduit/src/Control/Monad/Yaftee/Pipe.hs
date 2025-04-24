{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as F
import Control.HigherOpenUnion qualified as U
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type P i o = U.FromFirst (P_ i o)
data P_ i o r where Await :: P_ i o i; Yield :: forall i o . o -> P_ i o ()

(=$=) :: forall i a o es r r' . HFunctor.H (U.U es) =>
	Eff.E (P i a ': es) r -> Eff.E (P a o ': es) r' ->
	Eff.E (P i o ': es) (Eff.E (P i a ': es) r, Eff.E (P a o ': es) r')
o =$= p@(F.Pure _) = F.Pure (o, p)
o@(F.Pure _) =$= p = F.Pure (o, p)
o@(u F.:>>= q) =$= p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(_, Left v') ->
		U.weaken (HFunctor.map (o =$=) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$= (r `F.app` y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r `F.app`) =<< p')
			_ -> error "never occur"
	(_, Right (U.FromFirst (Yield ot) k)) ->
		U.inj (Yield @i ot) F.:>>= Q.singleton ((o =$=) . (r `F.app`) . k)
	(Right (U.FromFirst Await k), _) ->
		U.inj (Await @_ @o) F.:>>= Q.singleton ((=$= p) . (q `F.app`) . k)
	(Right (U.FromFirst (Yield ot) k), Right (U.FromFirst Await k')) ->
		(q `F.app` k ()) =$= (r `F.app` k' ot)
	(Left u', Right (U.FromFirst Await _)) ->
		U.weaken (HFunctor.map (=$= p) ((, p) . F.Pure) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q `F.app` x) =$= p'
			(o', p'@(F.Pure _)) -> F.Pure ((q `F.app`) =<< o', p')
			_ -> error "never occur"

(=@=) :: forall i a o es r r' . HFunctor.H (U.U es) =>
	Eff.E (P i a ': es) r -> Eff.E (P a o ': es) r' ->
	Eff.E (P i o ': es) (Eff.E (P i a ': es) r, Eff.E (P a o ': es) r')
o@(F.Pure _) =@= p = F.Pure (o, p)
o =@= p@(F.Pure _) = F.Pure (o, p)
o@(u F.:>>= q) =@= p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(Left u', _) ->
		U.weaken (HFunctor.map (=@= p) (\x -> (F.Pure x, p)) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q `F.app` x) =@= p'
			(o', p'@(F.Pure _)) -> F.Pure ((q `F.app`) =<< o', p')
			_ -> error "never occur"
	(_, Right (U.FromFirst (Yield ot) k)) ->
		U.inj (Yield @i ot) F.:>>= Q.singleton ((o =@=) . (r `F.app`) . k)
	(Right (U.FromFirst Await k), _) ->
		U.inj (Await @_ @o) F.:>>= Q.singleton ((=@= p) . (q `F.app`) . k)
	(Right (U.FromFirst (Yield ot) k), Right (U.FromFirst Await k')) ->
		(q `F.app` k ()) =@= (r `F.app` k' ot)
	(Right (U.FromFirst (Yield _) _), Left v') ->
		U.weaken (HFunctor.map (o =@=) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> ((o' =@=) . (r `F.app`)) y
			(o'@(F.Pure _), p') -> F.Pure (o', (r `F.app`) =<< p')
			_ -> error "never occur"
