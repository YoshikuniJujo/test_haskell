{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe where

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Data.FTCQueue qualified as Q

data P f i o a where
	Await :: P f i o i
	Yield :: forall f i o . o -> P f i o ()
	(:=$=) :: f i x r -> f x o r' -> P f i o (f i x r, f x o r')
	(:=@=) :: f i x r -> f x o r' -> P f i o (f i x r, f x o r')

await :: Union.Member P effs => Eff.E effs i o i
await = Eff.effh Await

yield :: Union.Member P effs => o -> Eff.E effs i o ()
yield = Eff.effh . Yield

(=$=) :: Union.Member P effs =>
	Eff.E effs i x r -> Eff.E effs x o r' ->
	Eff.E effs i o (Eff.E effs i x r, Eff.E effs x o r')
(=$=) = (Eff.effh .) . (:=$=)

(=@=) :: Union.Member P effs =>
	Eff.E effs i x r -> Eff.E effs x o r' ->
	Eff.E effs i o (Eff.E effs i x r, Eff.E effs x o r')
(=@=) = (Eff.effh .) . (:=@=)

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (P ': effs) i o a -> Eff.E effs i o (Maybe a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Just x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run Just u' HFreer.:>>=
			Q.singleton (maybe (pure Nothing) (run . (q `HFreer.app`)))
		Right Await -> pure Nothing
		Right (Yield _) -> pure Nothing
		Right (o :=$= p) -> run (o =$=! p) >>= maybe (pure Nothing) (run . (q `HFreer.app`))
		Right (o :=@= p) -> run (o =@=! p) >>= maybe (pure Nothing) (run . (q `HFreer.app`))

(=$=!) :: forall effs i x o r r' . Union.HFunctor (Union.U effs) =>
	Eff.E (P ': effs) i x r -> Eff.E (P ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (P ': effs) i x r, Eff.E (P ': effs) x o r')
o =$=! p@(HFreer.Pure _) = HFreer.Pure (o, p)
o@(HFreer.Pure _) =$=! p@(v HFreer.:>>= r) = case Union.decomp v of
	Left v' ->
		Union.weaken (Union.hmap (o =$=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		Q.singleton \case
			(o', HFreer.Pure y) -> o' =$=! (r `HFreer.app` y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', (r `HFreer.app`) =<< p')
			_ -> error "never occur"
	Right Await -> HFreer.Pure (o, p)
	Right (Yield ot) -> Union.injh (Yield @_ @i ot) HFreer.:>>=
		Q.singleton ((o =$=!) `HFreer.comp` r)
	Right (o' :=$= p') -> o =$=! ((r `HFreer.app`) =<< o' =$=! p')
	Right (o' :=@= p') -> o =$=! ((r `HFreer.app`) =<< o' =@=! p')
o@(u HFreer.:>>= q) =$=! p@(v HFreer.:>>= r) = case (Union.decomp u, Union.decomp v) of
	(_, Left v') ->
		Union.weaken (Union.hmap (o =$=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		Q.singleton \case
			(o', HFreer.Pure y) -> o' =$=! (r `HFreer.app` y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', (r `HFreer.app`) =<< p')
			_ -> error "never occur"
	(_, Right (Yield ot)) -> Union.injh (Yield @_ @i ot) HFreer.:>>=
		Q.singleton ((o =$=!) . (r `HFreer.app`))
	(Right Await, _) -> Union.injh (Await @_ @_ @o) HFreer.:>>=
		Q.singleton ((=$=! p) . (q `HFreer.app`))
	(Right (Yield ot), Right Await) -> (q `HFreer.app` ()) =$=! (r `HFreer.app` ot)
	(Left u', Right Await) ->
		Union.weaken (Union.hmap (=$=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> (q `HFreer.app` x) =$=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"
	(Right (o' :=$= p'), Right Await) -> ((q `HFreer.app`) =<< (o' =$=! p')) =$=! p
	(Right (o' :=@= p'), Right Await) -> ((q `HFreer.app`) =<< (o' =@=! p')) =$=! p
	(_, Right (o' :=$= p')) -> o =$=! ((r `HFreer.app`) =<< (o' =$=! p'))
	(_, Right (o' :=@= p')) -> o =$=! ((r `HFreer.app`) =<< (o' =@=! p'))

(=@=!) :: forall effs i x o r r' . Union.HFunctor (Union.U effs) =>
	Eff.E (P ': effs) i x r -> Eff.E (P ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (P ': effs) i x r, Eff.E (P ': effs) x o r')
o@(HFreer.Pure _) =@=! p = HFreer.Pure (o, p)
o@(u HFreer.:>>= q) =@=! p@(HFreer.Pure _) = case Union.decomp u of
	Left u' ->
		Union.weaken (Union.hmap (=@=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> q `HFreer.app` x =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"
	Right (Yield _) -> HFreer.Pure (o, p)
	Right Await -> Union.injh (Await @_ @_ @o) HFreer.:>>= Q.singleton ((=@=! p) . (q `HFreer.app`))
	Right (o' :=$= p') -> ((q `HFreer.app`) =<< (o' =$=! p')) =@=! p
	Right (o' :=@= p') -> ((q `HFreer.app`) =<< (o' =@=! p')) =@=! p
o@(u HFreer.:>>= q) =@=! p@(v HFreer.:>>= r) = case (Union.decomp u, Union.decomp v) of
	(Left u', _) ->
		Union.weaken (Union.hmap (=@=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> q `HFreer.app` x =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"
	(Right Await, _) -> Union.injh (Await @_ @_ @o) HFreer.:>>= Q.singleton ((=@=! p) . (q `HFreer.app`))
	(_, Right (Yield ot)) -> Union.injh (Yield @_ @i ot) HFreer.:>>=
		Q.singleton ((o =@=!) . (r `HFreer.app`))
	(Right (Yield ot), Right Await) -> q `HFreer.app` () =@=! (r `HFreer.app` ot)
	(Right (Yield _), Left v') ->
		Union.weaken (Union.hmap (o =@=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		Q.singleton \case
			(o', HFreer.Pure y) -> o' =@=! (r `HFreer.app` y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', (r `HFreer.app`) =<< p')
			_ -> error "never occur"
	(Right (Yield _), Right (o' :=$= p')) -> o =@=! ((r `HFreer.app`) =<< (o' =$=! p'))
	(Right (Yield _), Right (o' :=@= p')) -> o =@=! ((r `HFreer.app`) =<< (o' =@=! p'))
	(Right (o' :=$= p'), _) -> ((q `HFreer.app`) =<< (o' =$=! p')) =@=! p
	(Right (o' :=@= p'), _) -> ((q `HFreer.app`) =<< (o' =@=! p')) =@=! p
