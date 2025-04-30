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
import Yaftee.HFunctor qualified as Union
import Data.FTCQueue qualified as Q

data P f i o a where
	IsMore :: forall f i o a . (Bool -> a) -> P f i o a
	Await :: P f i o i
	Yield :: forall f i o a . o -> a -> P f i o a
	(:=$=) :: f i x r -> f x o r' -> ((f i x r, f x o r') -> a) -> P f i o a
	(:=@=) :: f i x r -> f x o r' -> P f i o (f i x r, f x o r')

instance Union.HFunctorSimple P where
	hmapS f g ((:=$=) o p k) = (f o :=$= f p) (\(x, y) -> k (g x, g y))

isEmpty :: Union.Member P effs => Eff.E effs i o Bool
isEmpty = not <$> Eff.effh (IsMore id)

isMore :: Union.Member P effs => Eff.E effs i o Bool
isMore = Eff.effh $ IsMore id

await :: Union.Member P effs => Eff.E effs i o i
await = Eff.effh Await

yield :: Union.Member P effs => o -> Eff.E effs i o ()
yield = Eff.effh . (`Yield` ())

(=$=) :: Union.Member P effs =>
	Eff.E effs i x r -> Eff.E effs x o r' ->
	Eff.E effs i o (Eff.E effs i x r, Eff.E effs x o r')
o =$= p = Eff.effh $ (o :=$= p) id

(=@=) :: Union.Member P effs =>
	Eff.E effs i x r -> Eff.E effs x o r' ->
	Eff.E effs i o (Eff.E effs i x r, Eff.E effs x o r')
(=@=) = (Eff.effh .) . (:=@=)

run :: Union.HFunctor' (Union.U effs) =>
	Eff.E (P ': effs) i o a -> Eff.E effs i o (Maybe a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Just x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap' run Just u' HFreer.:>>=
			Q.singleton (maybe (pure Nothing) (run . (q `HFreer.app`)))
		Right (IsMore _) -> pure Nothing
		Right Await -> pure Nothing
		Right (Yield _ _) -> pure Nothing
		Right ((:=$=) o p k) -> run (o =$=! p) >>= maybe (pure Nothing) (run . (q `HFreer.app`) . k)
		Right (o :=@= p) -> run (o =@=! p) >>= maybe (pure Nothing) (run . (q `HFreer.app`))

(=$=!) :: forall effs i x o r r' . Union.HFunctor' (Union.U effs) =>
	Eff.E (P ': effs) i x r -> Eff.E (P ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (P ': effs) i x r, Eff.E (P ': effs) x o r')
o =$=! p@(HFreer.Pure _) = HFreer.Pure (o, p)
o@(HFreer.Pure _) =$=! p@(v HFreer.:>>= r) = case Union.decomp v of
	Left v' ->
		Union.weaken (Union.hmap' (o =$=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		Q.singleton \case
			(o', HFreer.Pure y) -> o' =$=! (r `HFreer.app` y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', (r `HFreer.app`) =<< p')
			_ -> error "never occur"
	Right ((:=$=) o' p' k) -> o =$=! ((r `HFreer.app`) . k =<< o' =$=! p')
	Right (o' :=@= p') -> o =$=! ((r `HFreer.app`) =<< o' =@=! p')
	Right (IsMore k) -> o =$=! (r `HFreer.app` k False)
	Right Await -> HFreer.Pure (o, p)
	Right (Yield ot a) -> Union.injh (Yield @_ @i ot a) HFreer.:>>=
		Q.singleton ((o =$=!) `HFreer.comp` r)
o@(u HFreer.:>>= q) =$=! p@(v HFreer.:>>= r) = case (Union.decomp u, Union.decomp v) of
	(_, Left v') ->
		Union.weaken (Union.hmap' (o =$=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		Q.singleton \case
			(o', HFreer.Pure y) -> o' =$=! (r `HFreer.app` y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', (r `HFreer.app`) =<< p')
			_ -> error "never occur"
	(_, Right ((:=$=) o' p' k)) -> o =$=! ((r `HFreer.app`) . k =<< (o' =$=! p'))
	(_, Right (o' :=@= p')) -> o =$=! ((r `HFreer.app`) =<< (o' =@=! p'))
	(_, Right (Yield ot a)) -> Union.injh (Yield @_ @i ot a) HFreer.:>>=
		Q.singleton ((o =$=!) . (r `HFreer.app`))
	(Right (IsMore k), _) -> Union.injh (IsMore @_ @_ @o k) HFreer.:>>=
		Q.singleton ((=$=! p) . (q `HFreer.app`))
	(Right Await, _) -> Union.injh (Await @_ @_ @o) HFreer.:>>=
		Q.singleton ((=$=! p) . (q `HFreer.app`))
	(Right (Yield _ _), Right (IsMore k)) -> o =$=! (r `HFreer.app` k True)
	(Right ((:=$=) o' p' k), Right (IsMore _)) -> ((q `HFreer.app`) . k =<< (o' =$=! p')) =$=! p
	(Right (o' :=@= p'), Right (IsMore _)) -> ((q `HFreer.app`) =<< (o' =@=! p')) =$=! p
	(Right (Yield ot a), Right Await) -> (q `HFreer.app` a) =$=! (r `HFreer.app` ot)
	(Right ((:=$=) o' p' k), Right Await) -> ((q `HFreer.app`) . k =<< (o' =$=! p')) =$=! p
	(Right (o' :=@= p'), Right Await) -> ((q `HFreer.app`) =<< (o' =@=! p')) =$=! p
	(Left u', Right (IsMore _)) ->
		Union.weaken (Union.hmap' (=$=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> (q `HFreer.app` x) =$=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"
	(Left u', Right Await) ->
		Union.weaken (Union.hmap' (=$=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> (q `HFreer.app` x) =$=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"

(=@=!) :: forall effs i x o r r' . Union.HFunctor' (Union.U effs) =>
	Eff.E (P ': effs) i x r -> Eff.E (P ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (P ': effs) i x r, Eff.E (P ': effs) x o r')
o@(HFreer.Pure _) =@=! p = HFreer.Pure (o, p)
o@(u HFreer.:>>= q) =@=! p@(HFreer.Pure _) = case Union.decomp u of
	Left u' ->
		Union.weaken (Union.hmap' (=@=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> q `HFreer.app` x =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"
	Right ((:=$=) o' p' k) -> ((q `HFreer.app`) . k =<< (o' =$=! p')) =@=! p
	Right (o' :=@= p') -> ((q `HFreer.app`) =<< (o' =@=! p')) =@=! p
	Right (Yield _ _) -> HFreer.Pure (o, p)
	Right (IsMore k) -> Union.injh (IsMore @_ @_ @o k) HFreer.:>>= Q.singleton ((=@=! p) . (q `HFreer.app`))
	Right Await -> Union.injh (Await @_ @_ @o) HFreer.:>>= Q.singleton ((=@=! p) . (q `HFreer.app`))
o@(u HFreer.:>>= q) =@=! p@(v HFreer.:>>= r) = case (Union.decomp u, Union.decomp v) of
	(Left u', _) ->
		Union.weaken (Union.hmap' (=@=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		Q.singleton \case
			(HFreer.Pure x, p') -> q `HFreer.app` x =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure ((q `HFreer.app`) =<< o', p')
			_ -> error "never occur"
	(Right ((:=$=) o' p' k), _) -> ((q `HFreer.app`) . k =<< (o' =$=! p')) =@=! p
	(Right (o' :=@= p'), _) -> ((q `HFreer.app`) =<< (o' =@=! p')) =@=! p
	(Right (IsMore k), _) -> Union.injh (IsMore @_ @_ @o k) HFreer.:>>= Q.singleton ((=@=! p) . (q `HFreer.app`))
	(Right Await, _) -> Union.injh (Await @_ @_ @o) HFreer.:>>= Q.singleton ((=@=! p) . (q `HFreer.app`))
	(_, Right (Yield ot a)) -> Union.injh (Yield @_ @i ot a) HFreer.:>>=
		Q.singleton ((o =@=!) . (r `HFreer.app`))
	(Right (Yield _ _), Right (IsMore k)) -> o =@=! (r `HFreer.app` k True)
	(Right (Yield ot a), Right Await) -> q `HFreer.app` a =@=! (r `HFreer.app` ot)
	(Right (Yield _ _), Right ((:=$=) o' p' k)) -> o =@=! ((r `HFreer.app`) . k =<< (o' =$=! p'))
	(Right (Yield _ _), Right (o' :=@= p')) -> o =@=! ((r `HFreer.app`) =<< (o' =@=! p'))
	(Right (Yield _ _), Left v') ->
		Union.weaken (Union.hmap' (o =@=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		Q.singleton \case
			(o', HFreer.Pure y) -> o' =@=! (r `HFreer.app` y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', (r `HFreer.app`) =<< p')
			_ -> error "never occur"
