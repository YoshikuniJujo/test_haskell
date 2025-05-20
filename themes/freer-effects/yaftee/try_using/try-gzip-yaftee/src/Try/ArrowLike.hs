{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.ArrowLike (

	-- * ARROW LIKE

	first, first_, Second, secondEmpty,

	second, second_, First, firstEmpty,

	(***), (***.), (&&&), (&&&.)
	
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

first :: forall es i o a r r1 r2 . (
	U.Member Pipe.P es,
	U.Member (State.S (Second a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es (i, a) (o, a) (
		Eff.E es (i, a) o (Eff.E es (i, a) i r1, Eff.E es i o r),
		Eff.E es o (o, a) r2 )
first p = pre Pipe.=$= p Pipe.=$= post

first_ :: forall es i o a r . (
	U.Member Pipe.P es,
	U.Member (State.S (Second a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es (i, a) (o, a) ()
first_ = void . first

second :: forall es i o a r r1 r2 . (
	U.Member Pipe.P es,
	U.Member (State.S (First a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es (a, i) (a, o) (
		Eff.E es (a, i) o (Eff.E es (a, i) i r1, Eff.E es i o r),
		Eff.E es o (a, o) r2 )
second p = pre' Pipe.=$= p Pipe.=$= post'

second_ :: forall es i o a r . (
	U.Member Pipe.P es,
	U.Member (State.S (First a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es (a, i) (a, o) ()
second_ = void . second

(***) :: forall es i i' o o' r r' r1 r2 r3 r4 . (
	U.Member Pipe.P es,
	U.Member (State.S (Second i')) es,
	U.Member (State.S (First o)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es i' o' r' -> Eff.E es (i, i') (o, o') (
		Eff.E es (i, i') (o, i') (
			Eff.E es (i, i') o (Eff.E es (i, i') i r1, Eff.E es i o r),
			Eff.E es o (o, i') r2),
		Eff.E es (o, i') (o, o') (
			Eff.E es (o, i') o' (Eff.E es (o, i') i' r3, Eff.E es i' o' r'),
			Eff.E es o' (o, o') r4) )
p *** q = first p Pipe.=$= second q

(***.) :: forall es i i' o o' r r' . (
	U.Member Pipe.P es,
	U.Member (State.S (Second i')) es,
	U.Member (State.S (First o)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es i' o' r' -> Eff.E es (i, i') (o, o') ()
(***.) = (void .) . (***)


(&&&) :: forall es i o o' r r' r1 r2 r3 r4 r5 . (
	U.Member Pipe.P es,
	U.Member (State.S (Second i)) es,
	U.Member (State.S (First o)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es i o' r' -> Eff.E es i (o, o') (
		Eff.E es i (i, i) r1,
		Eff.E es (i, i) (o, o') (
			Eff.E es (i, i) (o, i) (
				Eff.E es (i, i) o (Eff.E es (i, i) i r2, Eff.E es i o r),
				Eff.E es o (o, i) r3),
			Eff.E es (o, i) (o, o') (
				Eff.E es (o, i) o' (Eff.E es (o, i) i r4, Eff.E es i o' r'),
				Eff.E es o' (o, o') r5)) )
p &&& q = dup Pipe.=$= (p *** q)

(&&&.) :: forall es i o o' r r' . (
	U.Member Pipe.P es,
	U.Member (State.S (Second i)) es,
	U.Member (State.S (First o)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es i o' r' -> Eff.E es i (o, o') ()
(&&&.) = (void .) . (&&&)

dup :: U.Member Pipe.P es => Eff.E es a (a, a) r
dup = forever $ Pipe.yield . (\x -> (x, x)) =<< Pipe.await

-- p *** q = first p Pipe.=$= swap Pipe.=$= first q Pipe.=$= swap

pre :: (
	U.Member Pipe.P es,
	U.Member (State.S (Second a)) es ) =>
	Eff.E es (i, a) i r
pre = forever do
	(i, x) <- Pipe.await
	push x
	Pipe.yield i

post :: (
	U.Member Pipe.P es,
	U.Member (State.S (Second a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es o (o, a) r
post = forever do
	o  <- Pipe.await
	x <- pop
	Pipe.yield (o, x)

newtype Second a = Second { unSecond :: [a] } deriving Show

secondEmpty :: Second a
secondEmpty = Second []

newtype First a = First { unFirst :: [a] } deriving Show

firstEmpty :: First a
firstEmpty = First []

pre' :: (
	U.Member Pipe.P es,
	U.Member (State.S (First a)) es ) =>
	Eff.E es (a, i) i r
pre' = forever do
	(x, i) <- Pipe.await
	push' x
	Pipe.yield i

post' :: (
	U.Member Pipe.P es,
	U.Member (State.S (First a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es o (a, o) r
post' = forever do
	o  <- Pipe.await
	x <- pop'
	Pipe.yield (x, o)

push :: forall a es i o . U.Member (State.S (Second a)) es =>
	a -> Eff.E es i o ()
push x = State.get @(Second a) >>= State.put . Second . (x :) . unSecond

pop :: forall a es i o . (
	U.Member (State.S (Second a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o a
pop = State.get >>= \case
	Second [] -> Except.throw "pop: never occur"
	Second (x : xs) -> x <$ State.put @(Second a) (Second xs)

push' :: forall a es i o . (U.Member (State.S (First a)) es) =>
	a -> Eff.E es i o ()
push' x = State.get @(First a) >>= State.put . First . (x :) . unFirst

pop' :: forall a es i o . (
	U.Member (State.S (First a)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o a
pop' = State.get >>= \case
	First [] -> Except.throw "pop: never occur"
	First (x : xs) -> x <$ State.put @(First a) (First xs)
