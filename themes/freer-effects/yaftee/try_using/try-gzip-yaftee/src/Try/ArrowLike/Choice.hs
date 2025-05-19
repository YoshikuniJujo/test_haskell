
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

import Debug.Trace

left :: (
	U.Member Pipe.P es,
	U.Member (State.S ([a], [a])) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (Either i a) (Either o a) (
	Eff.E es (Either i a) o (Eff.E es (Either i a) i (), Eff.E es i o r),
	Eff.E es o (Either o a) ())
left p = prec Pipe.=$= p Pipe.=$= postc

prec :: (
	U.Member Pipe.P es,
	U.Member (State.S ([a], [a])) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (Either i a) i ()
prec = do
	me <- Pipe.awaitMaybe
	maybe (pure ()) ((>> prec) . either Pipe.yield (trace "pushc" pushc)) me

postc :: (
	U.Member Pipe.P es,
	U.Member (State.S ([a], [a])) es
	) =>
	Eff.E es o (Either o a) ()
postc = do
	ml <- Pipe.awaitMaybe
	xs <- popAll
	(Pipe.yield . Right) `mapM_` xs
	maybe (pure ()) ((>> postc) . Pipe.yield . Left) ml

right :: (
	U.Member Pipe.P es,
	U.Member (State.S ([a], [a])) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (Either a i) (Either a o) (
		Eff.E es (Either a i) o (Eff.E es (Either a i) i (), Eff.E es i o r),
		Eff.E es o (Either a o) ())
right p = prec' Pipe.=$= p Pipe.=$= postc'

prec' :: (
	U.Member Pipe.P es,
	U.Member (State.S ([a], [a])) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (Either a i) i ()
prec' = do
	me <- Pipe.awaitMaybe
	maybe (pure ()) ((>> prec') . either pushc Pipe.yield) me

postc' :: (
	U.Member Pipe.P es,
	U.Member (State.S ([a], [a])) es
	) =>
	Eff.E es o (Either a o) ()
postc' = do
	mr <- Pipe.awaitMaybe
	xs <- popAll
	(Pipe.yield . Left) `mapM_` xs
	maybe
		(((Pipe.yield . Left) `mapM_`) =<< popAll)
		((>> postc') . Pipe.yield . Right) mr

(+++) :: (
	U.Member Pipe.P es,
	U.Member (State.S ([i'], [i'])) es,
	U.Member (State.S ([o], [o])) es,
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
	U.Member (State.S ([i'], [i'])) es,
	U.Member (State.S ([o], [o])) es,
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

pushc :: U.Member (State.S ([a], [a])) es => a -> Eff.E es i o ()
pushc x = State.modify (x `cons`)

popc :: U.Member (State.S ([a], [a])) es => Eff.E es i o (Maybe a)
popc = State.gets unsnoc >>= maybe (pure Nothing) \(q, x) -> Just x <$ State.put q

popAll :: U.Member (State.S ([a], [a])) es => Eff.E es i o [a]
popAll = maybe (pure []) (\x -> (x :) <$> popAll) =<< popc

cons :: a -> ([a], [a]) -> ([a], [a])
cons x (xs, ys) = (x : xs, ys)

unsnoc :: ([a], [a]) -> Maybe (([a], [a]), a)
unsnoc ([], []) = Nothing
unsnoc (xs, []) = unsnoc ([], reverse xs)
unsnoc (xs, y : ys) = Just ((xs, ys), y)
