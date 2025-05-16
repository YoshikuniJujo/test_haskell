{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.ArrowLike (

	-- * ARROW LIKE

	first, second, (***), (&&&),

	pre, first',

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

swap :: U.Member Pipe.P es => Eff.E es (a, b) (b, a) r
swap = forever $ Pipe.yield . (\(x, y) -> (y, x)) =<< Pipe.await

-- first :: U.Member Pipe.P es => Eff.E es i o r -> Eff.E es (i, a) (o, a) r
-- first p = forever 

first :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (i, a) (o, a) (
		Eff.E es (i, a) o (Eff.E es (i, a) i r0, Eff.E es i o r),
		Eff.E es o (o, a) r'' )
first p = pre Pipe.=$= p Pipe.=$= post

first' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (i, a) (o, a) (
		Eff.E es (i, a) i r0,
		Eff.E es i (o, a) (Eff.E es i o r, Eff.E es o (o, a) r'0) )
first' p = pre Pipe.=$= (p Pipe.=$= post)

second p = pre' Pipe.=$= p Pipe.=$= post'

(***) :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe i')) es,
	U.Member (State.S (Maybe o)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es i' o' r' -> Eff.E es (i, i') (o, o') (
		Eff.E es (i, i') (o, i') (
			Eff.E es (i, i') o (Eff.E es (i, i') i r00, Eff.E es i o r),
			Eff.E es o (o, i') r''0),
		Eff.E es (o, i') (o, o') (
			Eff.E es (o, i') o' (Eff.E es (o, i') i' r0, Eff.E es i' o' r'),
			Eff.E es o' (o, o') r'0) )
p *** q = first p Pipe.=$= second q

(&&&) :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe i)) es,
	U.Member (State.S (Maybe o)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es i o' r' -> Eff.E es i (o, o') (
		Eff.E es i (i, i) r0,
		Eff.E es (i, i) (o, o') (
			Eff.E es (i, i) (o, i) (
				Eff.E es (i, i) o (Eff.E es (i, i) i r000, Eff.E es i o r),
				Eff.E es o (o, i) r''00),
			Eff.E es (o, i) (o, o') (
				Eff.E es (o, i) o' (Eff.E es (o, i) i r00, Eff.E es i o' r'),
				Eff.E es o' (o, o') r'00)) )
p &&& q = dup Pipe.=$= (p *** q)

dup :: U.Member Pipe.P es => Eff.E es a (a, a) r
dup = forever $ Pipe.yield . (\x -> (x, x)) =<< Pipe.await

-- p *** q = first p Pipe.=$= swap Pipe.=$= first q Pipe.=$= swap

pre :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (i, a) i r
pre = forever do
	(i, x) <- Pipe.await
	push x
	Pipe.yield i

post :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es o (o, a) r
post = forever do
	o  <- Pipe.await
	x <- pop
	Pipe.yield (o, x)

pre' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (a, i) i r
pre' = forever do
	(x, i) <- Pipe.await
	push x
	Pipe.yield i

post' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es o (a, o) r
post' = forever do
	o  <- Pipe.await
	x <- pop
	Pipe.yield (x, o)

push :: forall a es i o . (
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es ) =>
	a -> Eff.E es i o ()
push x = State.get @(Maybe a) >>= \case
	Nothing -> State.put $ Just x
	Just _ -> Except.throw "push: never occur"

pop :: forall a es i o . (
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o a
pop = State.get >>= \case
	Nothing -> Except.throw "pop: never occur"
	Just x -> x <$ State.put @(Maybe a) Nothing

popMaybe :: forall a es i o .
	U.Member (State.S (Maybe a)) es =>
	Eff.E es i o (Maybe a)
popMaybe = State.get <* State.put @(Maybe a) Nothing

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
