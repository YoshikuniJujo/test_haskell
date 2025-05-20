
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
module Try.ArrowLike.Choice (

	-- * ARROW CHOICE LIKE

	left, Right, rightEmpty,
	right, Left, leftEmpty,

	(+++), (|||)
	
	) where

import Prelude hiding (Either(..))
import Prelude qualified as P
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U

left :: (
	U.Member Pipe.P es,
	U.Member (State.S (Right a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (P.Either i a) (P.Either o a) (
	Eff.E es (P.Either i a) o (Eff.E es (P.Either i a) i (), Eff.E es i o r),
	Eff.E es o (P.Either o a) ())
left p = do
	leftBegin
	prec Pipe.=$= p Pipe.=$= postc
		<* leftEnd

leftBegin, leftEnd :: forall a es i o . U.Member (State.S (Right a)) es =>
	Eff.E es (P.Either i a) (P.Either o a) ()
leftBegin = State.modify @(Right a) $ Right . (([], []) :) . unRight
leftEnd = State.modify @(Right a) $ Right . tail . unRight

prec :: (
	U.Member Pipe.P es,
	U.Member (State.S (Right a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (P.Either i a) i ()
prec = do
	me <- Pipe.awaitMaybe
	maybe (pure ()) ((>> prec) . either Pipe.yield pushc1L) me

postc :: (
	U.Member Pipe.P es,
	U.Member (State.S (Right a)) es
	) =>
	Eff.E es o (P.Either o a) ()
postc = do
	ml <- Pipe.awaitMaybe
	xs <- popAll1L
	(Pipe.yield . P.Right) `mapM_` xs
	maybe (pure ()) ((>> postc) . Pipe.yield . P.Left) ml

right :: (
	U.Member Pipe.P es,
	U.Member (State.S (Left a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es (P.Either a i) (P.Either a o) (
		Eff.E es (P.Either a i) o (Eff.E es (P.Either a i) i (), Eff.E es i o r),
		Eff.E es o (P.Either a o) ())
right p = do
	rightBegin
	prec' Pipe.=$= p Pipe.=$= postc'
		<* rightEnd

rightBegin, rightEnd :: forall a es i o .
	U.Member (State.S (Left a)) es =>
	Eff.E es (P.Either a i) (P.Either a o) ()
rightBegin = State.modify @(Left a) $ Left . (([], []) :) . unLeft
rightEnd = State.modify @(Left a) $ Left . tail . unLeft

prec' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Left a)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (P.Either a i) i ()
prec' = do
	me <- Pipe.awaitMaybe
	maybe (pure ()) ((>> prec') . either pushc1 Pipe.yield) me

postc' :: (
	U.Member Pipe.P es,
	U.Member (State.S (Left a)) es
	) =>
	Eff.E es o (P.Either a o) ()
postc' = do
	mr <- Pipe.awaitMaybe
	xs <- popAll1
	(Pipe.yield . P.Left) `mapM_` xs
	maybe
		(((Pipe.yield . P.Left) `mapM_`) =<< popAll1)
		((>> postc') . Pipe.yield . P.Right) mr

(+++) :: (
	U.Member Pipe.P es,
	U.Member (State.S (Right i')) es,
	U.Member (State.S (Left o)) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es i o r -> Eff.E es i' o' r' -> Eff.E es (P.Either i i') (P.Either o o') (
		Eff.E es (P.Either i i') (P.Either o i') (
			Eff.E es (P.Either i i') o (Eff.E es (P.Either i i') i (), Eff.E es i o r),
			Eff.E es o (P.Either o i') ()),
		Eff.E es (P.Either o i') (P.Either o o') (
			Eff.E es (P.Either o i') o' (Eff.E es (P.Either o i') i' (), Eff.E es i' o' r'),
			Eff.E es o' (P.Either o o') ()))
p +++ q = left p Pipe.=$= right q

(|||) :: (
	U.Member Pipe.P es,
	U.Member (State.S (Right i')) es,
	U.Member (State.S (Left o)) es,
	U.Member (Except.E String) es ) =>
	Eff.E es i o r -> Eff.E es i' o r' -> Eff.E es (P.Either i i') o (
		Eff.E es (P.Either i i') (P.Either o o) (
			Eff.E es (P.Either i i') (P.Either o i') (
				Eff.E es (P.Either i i') o (Eff.E es (P.Either i i') i (), Eff.E es i o r),
				Eff.E es o (P.Either o i') ()),
			Eff.E es (P.Either o i') (P.Either o o) (
				Eff.E es (P.Either o i') o (Eff.E es (P.Either o i') i' (), Eff.E es i' o r'),
				Eff.E es o (P.Either o o) ())
			),
		Eff.E es (P.Either o o) o r'0 )
p ||| q = (p +++ q) Pipe.=$= untag

untag :: U.Member Pipe.P es => Eff.E es (P.Either a a) a r
untag = forever $ Pipe.yield . either id id =<< Pipe.await

pushc1L :: U.Member (State.S (Right a)) es => a -> Eff.E es i o ()
pushc1L x = State.modify (x `consL`)

popc1L :: U.Member (State.S (Right a)) es => Eff.E es i o (Maybe a)
popc1L = State.gets unsnocL >>= maybe (pure Nothing) \(q, x) -> Just x <$ State.put q

popAll1L :: U.Member (State.S (Right a)) es => Eff.E es i o [a]
popAll1L = maybe (pure []) (\x -> (x :) <$> popAll1L) =<< popc1L

consL :: a -> Right a -> Right a
consL x (Right ((xs, ys) : qs)) = Right $ (x : xs, ys) : qs

unsnocL :: Right a -> Maybe (Right a, a)
unsnocL (Right (([], []) : _)) = Nothing
unsnocL (Right ((xs, []) : qs)) = unsnocL . Right $ ([], reverse xs) :qs
unsnocL (Right ((xs, y : ys) : qs)) = Just (Right $ (xs, ys) : qs, y)

newtype Right a = Right { unRight :: [([a], [a])] } deriving Show

rightEmpty :: Right a
rightEmpty = Right []

type QueueL a = [([a], [a])]

pushc1 :: U.Member (State.S (Left a)) es => a -> Eff.E es i o ()
pushc1 x = State.modify (x `cons`)

popc1 :: U.Member (State.S (Left a)) es => Eff.E es i o (Maybe a)
popc1 = State.gets unsnoc >>= maybe (pure Nothing) \(q, x) -> Just x <$ State.put q

popAll1 :: U.Member (State.S (Left a)) es => Eff.E es i o [a]
popAll1 = maybe (pure []) (\x -> (x :) <$> popAll1) =<< popc1

cons :: a -> Left a -> Left a
cons x (Left ((xs, ys) : qs)) = Left $ (x : xs, ys) : qs

unsnoc :: Left a -> Maybe (Left a, a)
unsnoc (Left (([], []) : _)) = Nothing
unsnoc (Left ((xs, []) : qs)) = unsnoc . Left $ ([], reverse xs) : qs
unsnoc (Left ((xs, y : ys) : qs)) = Just (Left $ (xs, ys) : qs, y)

newtype Left a = Left { unLeft :: [([a], [a])] } deriving Show

leftEmpty :: Left a
leftEmpty = Left []

type Queue a = [([a], [a])]
