{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe (

	-- * NORMAL

	P, await, yield, await', yield', run, (=$=), (=@=),

	-- * NAMED

	Named, awaitN, yieldN, runN, (=$=.), (=@=.)

	) where

import GHC.TypeLits
import Control.Arrow
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as F
import Control.HigherOpenUnion qualified as U
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- * NORMAL

type P i o = Named "" i o

await :: forall i o es . Eff.E (P i o ': es) i
await = await' o

yield :: forall i o es . o -> Eff.E (P i o ': es) ()
yield = yield' i

await' :: forall i es . forall o -> U.Member (P i o) es => Eff.E es i
await' = awaitN ""

yield' :: forall o es . forall i -> U.Member (P i o) es => o -> Eff.E es ()
yield' = yieldN ""

run :: HFunctor.H (U.U es) =>
	Eff.E (P i o ': es) a -> Eff.E es (Maybe (a, [o]))
run = runN

(=$=) :: forall i a o es r r' . HFunctor.H (U.U es) =>
	Eff.E (P i a ': es) r -> Eff.E (P a o ': es) r' ->
	Eff.E (P i o ': es) (Eff.E (P i a ': es) r, Eff.E (P a o ': es) r')
(=$=) = (=$=.)

(=@=) :: forall i a o es r r' . HFunctor.H (U.U es) =>
	Eff.E (P i a ': es) r -> Eff.E (P a o ': es) r' ->
	Eff.E (P i o ': es) (Eff.E (P i a ': es) r, Eff.E (P a o ': es) r')
(=@=) = (=@=.)

-- * NAMED

type Named nm i o = U.FromFirst (Named_ nm i o)
data Named_ (nm :: Symbol) i o r where
	Await :: Named_ nm i o i
	Yield :: forall nm i o . o -> Named_ nm i o ()

awaitN :: forall nm o -> U.Member (Named nm i o) es => Eff.E es i
awaitN nm o = Eff.eff (Await @nm @_ @o)

yieldN :: forall nm i -> U.Member (Named nm i o) es => o -> Eff.E es ()
yieldN nm i = Eff.eff . Yield @nm @i

runN :: HFunctor.H (U.U es) =>
	Eff.E (Named nm i o ': es) a -> Eff.E es (Maybe (a, [o]))
runN = \case
	F.Pure x -> F.Pure $ Just (x, [])
	u F.:>>= q -> case U.decomp u of
		Left u' -> HFunctor.map runN (Just . (, [])) u' F.:>>=
			Q.singleton \case
				Nothing -> F.Pure Nothing
				Just (x, os) -> (second (os ++) <$>) <$> (runN `F.comp` q) x
		Right (U.FromFirst Await _) -> F.Pure Nothing
		Right (U.FromFirst (Yield o) k) -> (((o :) `second`) <$>) <$> runN (q `F.app` k ())

(=$=.) :: forall nmix nmxo nmio i a o es r r' . HFunctor.H (U.U es) =>
	Eff.E (Named nmix i a ': es) r -> Eff.E (Named nmxo a o ': es) r' ->
	Eff.E (Named nmio i o ': es) (Eff.E (Named nmix i a ': es) r, Eff.E (Named nmxo a o ': es) r')
o =$=. p@(F.Pure _) = F.Pure (o, p)
o@(F.Pure _) =$=. p@(v F.:>>= r) = case U.decomp v of
	Left v' ->
		U.weaken (HFunctor.map (o =$=.) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$=. (r `F.app` y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r `F.app`) =<< p')
			_ -> error "never occur"
	Right (U.FromFirst Await _) -> F.Pure (o, p)
	Right (U.FromFirst (Yield ot) k) ->
		U.inj (Yield @nmio @i ot) F.:>>= Q.singleton ((o =$=.) . (r `F.app`) . k)
o@(u F.:>>= q) =$=. p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(_, Left v') ->
		U.weaken (HFunctor.map (o =$=.) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> o' =$=. (r `F.app` y)
			(o'@(F.Pure _), p') -> F.Pure (o', (r `F.app`) =<< p')
			_ -> error "never occur"
	(_, Right (U.FromFirst (Yield ot) k)) ->
		U.inj (Yield @nmio @i ot) F.:>>= Q.singleton ((o =$=.) . (r `F.app`) . k)
	(Right (U.FromFirst Await k), _) ->
		U.inj (Await @nmio @_ @o) F.:>>= Q.singleton ((=$=. p) . (q `F.app`) . k)
	(Right (U.FromFirst (Yield ot) k), Right (U.FromFirst Await k')) ->
		(q `F.app` k ()) =$=. (r `F.app` k' ot)
	(Left u', Right (U.FromFirst Await _)) ->
		U.weaken (HFunctor.map (=$=. p) ((, p) . F.Pure) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q `F.app` x) =$=. p'
			(o', p'@(F.Pure _)) -> F.Pure ((q `F.app`) =<< o', p')
			_ -> error "never occur"

(=@=.) :: forall nmix nmxo nmio i a o es r r' . HFunctor.H (U.U es) =>
	Eff.E (Named nmix i a ': es) r -> Eff.E (Named nmxo a o ': es) r' ->
	Eff.E (Named nmio i o ': es) (Eff.E (Named nmix i a ': es) r, Eff.E (Named nmxo a o ': es) r')
o@(F.Pure _) =@=. p = F.Pure (o, p)
o@(u F.:>>= q) =@=. p@(F.Pure _) = case U.decomp u of
	Left u' ->
		U.weaken (HFunctor.map (=@=. p) (\x -> (F.Pure x, p)) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q `F.app` x) =@=. p'
			(o', p'@(F.Pure _)) -> F.Pure ((q `F.app`) =<< o', p')
			_ -> error "never occur"
	Right (U.FromFirst Await k) ->
		U.inj (Await @nmio @_ @o) F.:>>= Q.singleton ((=@=. p) . (q `F.app`) . k)
	Right (U.FromFirst (Yield _) _) -> F.Pure (o, p)
o@(u F.:>>= q) =@=. p@(v F.:>>= r) = case (U.decomp u, U.decomp v) of
	(Left u', _) ->
		U.weaken (HFunctor.map (=@=. p) (\x -> (F.Pure x, p)) u') F.:>>=
		Q.singleton \case
			(F.Pure x, p') -> (q `F.app` x) =@=. p'
			(o', p'@(F.Pure _)) -> F.Pure ((q `F.app`) =<< o', p')
			_ -> error "never occur"
	(_, Right (U.FromFirst (Yield ot) k)) ->
		U.inj (Yield @nmio @i ot) F.:>>= Q.singleton ((o =@=.) . (r `F.app`) . k)
	(Right (U.FromFirst Await k), _) ->
		U.inj (Await @nmio @_ @o) F.:>>= Q.singleton ((=@=. p) . (q `F.app`) . k)
	(Right (U.FromFirst (Yield ot) k), Right (U.FromFirst Await k')) ->
		(q `F.app` k ()) =@=. (r `F.app` k' ot)
	(Right (U.FromFirst (Yield _) _), Left v') ->
		U.weaken (HFunctor.map (o =@=.) ((o ,) . F.Pure) v') F.:>>=
		Q.singleton \case
			(o', F.Pure y) -> ((o' =@=.) . (r `F.app`)) y
			(o'@(F.Pure _), p') -> F.Pure (o', (r `F.app`) =<< p')
			_ -> error "never occur"
