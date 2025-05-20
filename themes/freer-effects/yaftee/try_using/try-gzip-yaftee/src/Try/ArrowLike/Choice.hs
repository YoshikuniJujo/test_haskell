
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
module Try.ArrowLike.Choice (

	-- * ARROW CHOICE LIKE

	left, right, (+++), (|||)
	
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

left :: (
	U.Member Pipe.P es,
	U.Member (State.S (QueueL a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (Either i a) (Either o a) (
	Eff.E es (Either i a) o (Eff.E es (Either i a) i (), Eff.E es i o r),
	Eff.E es o (Either o a) ())
left p = do
	leftBegin
	prec Pipe.=$= p Pipe.=$= postc
		<* leftEnd

leftBegin, leftEnd :: forall a es i o . U.Member (State.S (QueueL a)) es =>
	Eff.E es (Either i a) (Either o a) ()
leftBegin = State.modify @(QueueL a) (([], []) :)
leftEnd = State.modify @(QueueL a) tail

prec :: (
	U.Member Pipe.P es,
	U.Member (State.S (QueueL a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (Either i a) i ()
prec = do
	me <- Pipe.awaitMaybe
	maybe (pure ()) ((>> prec) . either Pipe.yield pushc1L) me

postc :: (
	U.Member Pipe.P es,
	U.Member (State.S (QueueL a)) es
	) =>
	Eff.E es o (Either o a) ()
postc = do
	ml <- Pipe.awaitMaybe
	xs <- popAll1L
	(Pipe.yield . Right) `mapM_` xs
	maybe (pure ()) ((>> postc) . Pipe.yield . Left) ml

right :: (
	U.Member Pipe.P es,
	U.Member (State.S (Queue a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (Either a i) (Either a o) (
		Eff.E es (Either a i) o (Eff.E es (Either a i) i (), Eff.E es i o r),
		Eff.E es o (Either a o) ())
right p = prec' Pipe.=$= p Pipe.=$= postc'

prec' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Queue a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (Either a i) i ()
prec' = do
	me <- Pipe.awaitMaybe
	maybe (pure ()) ((>> prec') . either pushc1 Pipe.yield) me

postc' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Queue a)) es
	) =>
	Eff.E es o (Either a o) ()
postc' = do
	mr <- Pipe.awaitMaybe
	xs <- popAll1
	(Pipe.yield . Left) `mapM_` xs
	maybe
		(((Pipe.yield . Left) `mapM_`) =<< popAll1)
		((>> postc') . Pipe.yield . Right) mr

(+++) :: (
	U.Member Pipe.P es,
	U.Member (State.S (QueueL i')) es,
	U.Member (State.S (Queue o)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es i' o' r' -> Eff.E es (Either i i') (Either o o') (
		Eff.E es (Either i i') (Either o i') (
			Eff.E es (Either i i') o (Eff.E es (Either i i') i (), Eff.E es i o r),
			Eff.E es o (Either o i') ()),
		Eff.E es (Either o i') (Either o o') (
			Eff.E es (Either o i') o' (Eff.E es (Either o i') i' (), Eff.E es i' o' r'),
			Eff.E es o' (Either o o') ()))
p +++ q = left p Pipe.=$= right q

(|||) :: (
	U.Member Pipe.P es,
	U.Member (State.S (QueueL i')) es,
	U.Member (State.S (Queue o)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es i' o r' -> Eff.E es (Either i i') o (
		Eff.E es (Either i i') (Either o o) (
			Eff.E es (Either i i') (Either o i') (
				Eff.E es (Either i i') o (Eff.E es (Either i i') i (), Eff.E es i o r),
				Eff.E es o (Either o i') ()),
			Eff.E es (Either o i') (Either o o) (
				Eff.E es (Either o i') o (Eff.E es (Either o i') i' (), Eff.E es i' o r'),
				Eff.E es o (Either o o) ())
			),
		Eff.E es (Either o o) o r'0 )
p ||| q = (p +++ q) Pipe.=$= untag

untag :: U.Member Pipe.P es => Eff.E es (Either a a) a r
untag = forever $ Pipe.yield . either id id =<< Pipe.await

pushc1L :: U.Member (State.S (QueueL a)) es => a -> Eff.E es i o ()
pushc1L x = State.modify (x `consL`)

popc1L :: U.Member (State.S (QueueL a)) es => Eff.E es i o (Maybe a)
popc1L = State.gets unsnocL >>= maybe (pure Nothing) \(q, x) -> Just x <$ State.put q

popAll1L :: U.Member (State.S (QueueL a)) es => Eff.E es i o [a]
popAll1L = maybe (pure []) (\x -> (x :) <$> popAll1L) =<< popc1L

consL :: a -> QueueL a -> QueueL a
consL x ((xs, ys) : qs) = (x : xs, ys) : qs

unsnocL :: QueueL a -> Maybe (QueueL a, a)
unsnocL (([], []) : _) = Nothing
unsnocL ((xs, []) : qs) = unsnocL $ ([], reverse xs) :qs
unsnocL ((xs, y : ys) : qs) = Just ((xs, ys) : qs, y)

type QueueL a = [([a], [a])]

pushc1 :: U.Member (State.S (Queue a)) es => a -> Eff.E es i o ()
pushc1 x = State.modify (x `cons`)

popc1 :: U.Member (State.S (Queue a)) es => Eff.E es i o (Maybe a)
popc1 = State.gets unsnoc >>= maybe (pure Nothing) \(q, x) -> Just x <$ State.put q

popAll1 :: U.Member (State.S (Queue a)) es => Eff.E es i o [a]
popAll1 = maybe (pure []) (\x -> (x :) <$> popAll1) =<< popc1

cons :: a -> Queue a -> Queue a
cons x (xs, ys) = (x : xs, ys)

unsnoc :: Queue a -> Maybe (Queue a, a)
unsnoc ([], []) = Nothing
unsnoc (xs, []) = unsnoc ([], reverse xs)
unsnoc (xs, y : ys) = Just ((xs, ys), y)

type Queue a = ([a], [a])
