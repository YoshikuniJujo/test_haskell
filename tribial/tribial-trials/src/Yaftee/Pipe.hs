{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Pipe where

import Data.Kind

import Yaftee.Eff qualified as Eff
import Yaftee.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.TypeElem qualified as Elem
import Yaftee.HFunctor qualified as Union

data P (f :: Type -> Type -> Type -> Type) i o a where
	Await :: P f i o i
	Yield :: forall f i o . o -> P f i o ()
	(:=$=) :: f i x r -> f x o r' -> P f i o (f i x r, f x o r')
	(:=@=) :: f i x r -> f x o r' -> P f i o (f i x r, f x o r')

await :: Elem.Member P effs => Eff.E effs i o i
await = Eff.effh Await

yield :: Elem.Member P effs => o -> Eff.E effs i o ()
yield = Eff.effh . Yield

(=$=) :: Elem.Member P effs =>
	Eff.E effs i x r -> Eff.E effs x o r' ->
	Eff.E effs i o (Eff.E effs i x r, Eff.E effs x o r')
i =$= o = Eff.effh $ i :=$= o

(=@=) :: Elem.Member P effs =>
	Eff.E effs i x r -> Eff.E effs x o r' ->
	Eff.E effs i o (Eff.E effs i x r, Eff.E effs x o r')
i =@= o = Eff.effh $ i :=@= o

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (P ': effs) i o a -> Eff.E effs i o (Maybe a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Just x
	u HFreer.:>>= k -> case Union.decomp u of
		Left u' -> Union.hmap run Just u' HFreer.:>>= maybe (pure Nothing) (run . k)
		Right Await -> pure Nothing
		Right (Yield _) -> pure Nothing
		Right (o :=$= p) -> run (o =$=! p) >>= maybe (pure Nothing) (run . k)
		Right (o :=@= p) -> run (o =@=! p) >>= maybe (pure Nothing) (run . k)

(=$=!) :: forall effs i x o r r' . Union.HFunctor (Union.U effs) =>
	Eff.E (P ': effs) i x r -> Eff.E (P ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (P ': effs) i x r, Eff.E (P ': effs) x o r')
o =$=! p@(HFreer.Pure _) = HFreer.Pure (o, p)
o@(HFreer.Pure _) =$=! p@(v HFreer.:>>= r) = case Union.decomp v of
	Left v' ->
		Union.weaken (Union.hmap (o =$=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =$=! (r y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', r =<< p')
			_ -> error "never occur"
	Right Await -> HFreer.Pure (o, p)
	Right (Yield ot) -> Union.injh (Yield @_ @i ot) HFreer.:>>= ((o =$=!) . r)
	Right (o' :=$= p') -> o =$=! (r =<< (o' =$=! p'))
	Right (o' :=@= p') -> o =$=! (r =<< (o' =@=! p'))
o@(u HFreer.:>>= k) =$=! p@(v HFreer.:>>= l) = case (Union.decomp u, Union.decomp v) of
	(_, Left v') ->
		Union.weaken (Union.hmap (o =$=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =$=! (l y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"
	(_, Right (Yield ot)) -> Union.injh (Yield @_ @i ot) HFreer.:>>= ((o =$=!) . l)
	(Right Await, _) -> Union.injh (Await @_ @i @o) HFreer.:>>= ((=$=! p) . k)
	(Right (Yield ot), Right Await) -> k () =$=! l ot
	(Right (o' :=$= p'), Right Await) -> (k =<< (o' =$=! p')) =$=! p
	(Right (o' :=@= p'), Right Await) -> (k =<< (o' =@=! p')) =$=! p
	(_, Right (o' :=$= p')) -> o =$=! (l =<< (o' =$=! p'))
	(_, Right (o' :=@= p')) -> o =$=! (l =<< (o' =@=! p'))
	(Left u', Right Await) ->
		Union.weaken (Union.hmap (=$=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		\case	(HFreer.Pure x, p') -> k x =$=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"

(=@=!) :: forall effs i x o r r' . Union.HFunctor (Union.U effs) =>
	Eff.E (P ': effs) i x r -> Eff.E (P ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (P ': effs) i x r, Eff.E (P ': effs) x o r')
o@(HFreer.Pure _) =@=! p = HFreer.Pure (o, p)
o@(u HFreer.:>>= k) =@=! p@(HFreer.Pure _) = case Union.decomp u of
	Left u' ->
		Union.weaken (Union.hmap (=@=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		\case	(HFreer.Pure x, p') -> k x =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"
	Right (Yield _) -> HFreer.Pure (o, p)
	Right Await -> Union.injh (Await @_ @_ @o) HFreer.:>>= ((=@=! p) . k)
	Right (o' :=$= p') -> (k =<< (o' =$=! p')) =@=! p
	Right (o' :=@= p') -> (k =<< (o' =@=! p')) =@=! p
o@(u HFreer.:>>= k) =@=! p@(v HFreer.:>>= l) = case (Union.decomp u, Union.decomp v) of
	(Left u', _) ->
		Union.weaken (Union.hmap (=@=! p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		\case	(HFreer.Pure x, p') -> k x =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"
	(Right Await, _) -> Union.injh (Await @_ @_ @o) HFreer.:>>= ((=@=! p) . k)
	(_, Right (Yield ot)) -> Union.injh (Yield @_ @i ot) HFreer.:>>= ((o =@=!) . l)
	(Right (Yield ot), Right Await) -> k () =@=! l ot
	(Right (Yield _), Right (o' :=$= p')) -> o =@=! (l =<< (o' =$=! p'))
	(Right (Yield _), Right (o' :=@= p')) -> o =@=! (l =<< (o' =@=! p'))
	(Right (o' :=$= p'), _) -> (k =<< (o' =$=! p')) =@=! p
	(Right (o' :=@= p'), _) -> (k =<< (o' =@=! p')) =@=! p
	(Right (Yield _), Left v') ->
		Union.weaken (Union.hmap (o =@=!) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =@=! (l y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"
