{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont
import Data.Typeable

import TypeElem

data VE r a
	= Val a
	| forall t . (Functor t, Typeable t, Typeable a) => E (t (VE r a))
	deriving Typeable

class (Functor e, Typeable e) => Effect e where
	toEffect :: Typeable a => e (VE r a) -> VE r a
	fromEffect :: Typeable r => VE r a -> Maybe (e (VE r a))

	toEffect = E
	fromEffect = \case
		E e -> cast e
		Val _ -> Nothing

newtype Reader e v = Reader (e -> v)
	deriving (Typeable, Functor)

instance Typeable e => Effect (Reader e) where

ask :: (Member (Reader e) r, Typeable e, Typeable a) => Cont (VE r a) e
ask = cont $ toEffect . Reader

runReader :: (Typeable r, Typeable e) =>
	Cont (VE (Reader e :> r) a) a -> e -> VE r a
runReader m e = rloop (runCont m Val) e

runReader2 :: (Typeable r, Typeable e) =>
	Cont (VE (Reader e :> r) a) a -> e -> Cont (VE r a) a
runReader2 m = cont . const . runReader m

rloop :: (Typeable r, Typeable e) => VE (Reader e :> r) a -> e -> VE r a
rloop m e = case m of
	Val x -> Val x
	E u -> case fromEffect m of
		Just (Reader r) -> rloop (r e) e
		Nothing -> E $ fmap (`rloop` e) u

run :: VE () a -> a
run (Val x) = x
run _ = undefined

run2 :: Cont (VE () a) a -> a
run2 m = case runCont m Val of
	Val x -> x
	_ -> undefined

data State s a = State (s -> s) (s -> a)
	deriving (Typeable, Functor)

instance Typeable s => Effect (State s) where

modify :: (Member (State s) r, Typeable s, Typeable a) =>
	(s -> s) -> Cont (VE r a) s
modify f = cont $ toEffect . State f

get :: (Member (State s) r, Typeable s, Typeable a) => Cont (VE r a) s
get = cont $ toEffect . State id

runState ::
	(Typeable r, Typeable s) => Cont (VE (State s :> r) a) a -> s -> VE r a
runState m s = sloop (runCont m Val) s

runState2 :: (Typeable r, Typeable s) =>
	Cont (VE (State s :> r) a) a -> s -> Cont (VE r a) a
runState2 m = cont . const . runState m

sloop :: (Typeable r, Typeable s) => VE (State s :> r) a -> s -> VE r a
sloop m s = case m of
	Val x -> Val x
	E u -> case fromEffect m of
		Just (State f k) -> sloop (k s) (f s)
		Nothing -> E $ fmap (`sloop` s) u
