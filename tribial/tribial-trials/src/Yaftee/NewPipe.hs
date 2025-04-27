{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.NewPipe where

import Data.Kind
import Yaftee.Eff qualified as Eff
import Yaftee.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union

data Yield (f :: Type -> Type -> Type -> Type) i o a where
	Yield :: forall f i o a . o -> a -> Yield f i o a

type family Yieldable (f :: Type -> Type -> Type -> Type) ::
	Type -> Type -> Type -> Type

type instance Yieldable (Eff.E (P ': Yield ': Await ': effs)) = Eff.E (Yield ': Await ': effs)

data Await (f :: Type -> Type -> Type -> Type) i o a where
	Await :: forall f i o a . (i -> a) -> Await f i o a

type family Awaitable (f :: Type -> Type -> Type -> Type) ::
	Type -> Type -> Type -> Type

type instance Awaitable (Eff.E (P ': Yield ': Await ': effs)) = Eff.E (Await ': Yield ': effs)

data P (f :: Type -> Type -> Type -> Type) i o a where
	(:=$=) :: Yieldable f i x r -> Awaitable f x o r' ->
		P f i o (Yieldable f i x r, Awaitable f x o r')
	(:=@=) :: Yieldable f i x r -> Awaitable f x o r' ->
		P f i o (Yieldable f i x r, Awaitable f x o r')

yield :: Union.Member Yield effs => o -> Eff.E effs i o ()
yield = Eff.effh . (`Yield` ())

await :: Union.Member Await effs => Eff.E effs i o i
await = Eff.effh $ Await id

(=$=) :: Union.Member P effs =>
	Yieldable (Eff.E effs) i x r -> Awaitable (Eff.E effs) x o r ->
	Eff.E effs i o (Yieldable (Eff.E effs) i x r, Awaitable (Eff.E effs) x o r)
{-
(=$=) :: Eff.E (Yield ': effs) i x r -> Eff.E (Await ': effs) x o r' ->
	Eff.E (P ': effs) i o (Eff.E (Yield ': effs) i x r, Eff.E (Await ': effs) x o r')
	-}
o =$= p = Eff.effh $ o :=$= p

(=@=) :: Union.Member P effs =>
	Yieldable (Eff.E effs) i x r -> Awaitable (Eff.E effs) x o r' ->
	Eff.E effs i o (Yieldable (Eff.E effs) i x r, Awaitable (Eff.E effs) x o r')
o =@= p = Eff.effh $ o :=@= p

run :: Union.HFunctor (Union.U effs) =>
--	Eff.E (P ': effs) i o a -> Eff.E effs i o (Eff.E (Yield : effs) i x r, Eff.E (Await : effs) x o r')
	Eff.E (P ': Yield ': Await ': effs) i o a -> Eff.E (Yield ': Await ': effs) i o a
run = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= k -> case Union.decomp u of
		Left u' -> Union.hmapOI run id u' HFreer.:>>= (run . k)
		Right (o :=$= p) -> (o =$=!. p) >>= (run . k)
--		Right (o :=@= p) -> (o =@=! p) >>= (run . k)

(=$=!) :: Union.HFunctor (Union.U effs) =>
	Eff.E (Yield ': effs) i x r -> Eff.E (Await ': effs) x o r' ->
	Eff.E effs i o (Eff.E (Yield ': effs) i x r, Eff.E (Await ': effs) x o r')
o =$=! p@(HFreer.Pure _) = HFreer.Pure (o, p)
o@(HFreer.Pure _) =$=! p@(v HFreer.:>>= l) = case Union.decomp v of
	Left v' -> Union.hmap (o =$=!) ((o ,) . HFreer.Pure) v' HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =$=! (l y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"
	Right (Await _) -> HFreer.Pure (o, p)
o@(u HFreer.:>>= k) =$=! p@(v HFreer.:>>= l) = case (Union.decomp u, Union.decomp v) of
	(_, Left v') -> Union.hmap (o =$=!) ((o ,) . HFreer.Pure) v' HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =$=! (l y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"
	(Right (Yield ot a), Right (Await c)) -> k a =$=! l (c ot)
	(Left u', Right (Await _)) ->
		Union.hmap (=$=! p) ((, p) . HFreer.Pure) u' HFreer.:>>=
		\case	(HFreer.Pure x, p') -> k x =$=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"

(=$=!.) :: Union.HFunctor (Union.U effs) =>
	Eff.E (Yield ': Await ': effs) i x r -> Eff.E (Await ': Yield ': effs) x o r' ->
	Eff.E (Yield ': Await ': effs) i o (
		Eff.E (Yield ': Await ': effs) i x r, Eff.E (Await ': Yield ': effs) x o r'
		)
o =$=!. p@(HFreer.Pure _) = HFreer.Pure (o, p)
o@(HFreer.Pure _) =$=!. p@(v HFreer.:>>= l) = case Union.decomp v of 
	Left v' -> Union.weaken1 (Union.hmapO (o =$=!.) ((o ,) . HFreer.Pure) v') HFreer.:>>=
--	Left v' -> (Union.hmap (o =$=!.) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =$=!. (l y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"
	Right (Await _) -> HFreer.Pure (o, p)
o@(u HFreer.:>>= k) =$=!. p@(v HFreer.:>>= l) = case (Union.decomp u, Union.decomp v) of
	(_, Left v') -> Union.weaken1 (Union.hmapO (o =$=!.) ((o ,) . HFreer.Pure) v') HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =$=!. (l y)
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"
	(Right (Yield ot a), Right (Await c)) -> k a =$=!. l (c ot)
	(Left u', Right (Await _)) ->
		Union.weaken (Union.hmapI (=$=!. p) ((, p) . HFreer.Pure) u') HFreer.:>>=
		\case	(HFreer.Pure x, p') -> k x =$=!. p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"

(=@=!) :: Union.HFunctor (Union.U effs) =>
	Eff.E (Yield ': effs) i x r -> Eff.E (Await ': effs) x o r' ->
	Eff.E effs i o (Eff.E (Yield ': effs) i x r, Eff.E (Await ': effs) x o r')
o@(HFreer.Pure _) =@=! p = HFreer.Pure (o, p)
o@(u HFreer.:>>= k) =@=! p@(HFreer.Pure _) = case Union.decomp u of
	Left u' -> Union.hmap (=@=! p) (\x -> (HFreer.Pure x, p)) u' HFreer.:>>=
		\case	(HFreer.Pure x, p') -> (k x) =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"
	Right (Yield _ _) -> HFreer.Pure (o, p)
o@(u HFreer.:>>= k) =@=! p@(v HFreer.:>>= l) = case (Union.decomp u, Union.decomp v) of
	(Left u', _) -> Union.hmap (=@=! p) (\x -> (HFreer.Pure x, p)) u' HFreer.:>>=
		\case	(HFreer.Pure x, p') -> (k x) =@=! p'
			(o', p'@(HFreer.Pure _)) -> HFreer.Pure (k =<< o', p')
			_ -> error "never occur"
	(Right (Yield ot a), Right (Await c)) -> k a =@=! l (c ot)
	(Right (Yield _ _), Left v') ->
		Union.hmap (o =@=!) ((o ,) . HFreer.Pure) v' HFreer.:>>=
		\case	(o', HFreer.Pure y) -> o' =@=! l y
			(o'@(HFreer.Pure _), p') -> HFreer.Pure (o', l =<< p')
			_ -> error "never occur"

instance Union.HFunctorO Yield where
	hmapO _ g (Yield o a) = Yield o $ g a

instance Union.HFunctorI Await where
	hmapI _ g (Await c) = Await $ g . c
