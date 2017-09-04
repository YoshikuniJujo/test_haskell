{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.State (State, runState, runState2, modify, get) where

import Control.Monad.Cont
import Data.Typeable

import MyEff

data State s a = State (s -> s) (s -> a)
	deriving (Typeable, Functor)

modify :: (Member (State s) r, Typeable s) =>
	(s -> s) -> Cont (VE r a) s
modify f = cont $ toEffect . State f

get :: (Member (State s) r, Typeable s) => Cont (VE r a) s
get = cont $ toEffect . State id

runState ::
	(Typeable r, Typeable s) => Cont (VE (State s :> r) a) a -> s -> VE r a
runState m = sloop (runCont m V)

runState2 :: (Typeable r, Typeable s) =>
	Cont (VE (State s :> r) a) a -> s -> Cont (VE r a) a
runState2 m = cont . const . runState m

sloop :: (Typeable r, Typeable s) => VE (State s :> r) a -> s -> VE r a
sloop m s = case m of
	V x -> V x
	E u -> case fromEffect m of
		Just (State f k) -> sloop (k s) (f s)
		Nothing -> E $ fmap (`sloop` s) u
