{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.ArrowLike (

	-- * ARROW LIKE

	first, second, (***), (&&&),
	
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

first :: (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (i, a) (o, a) (
		Eff.E es (i, a) o (Eff.E es (i, a) i r0, Eff.E es i o r),
		Eff.E es o (o, a) r'' )
first p = pre Pipe.=$= p Pipe.=$= post

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
