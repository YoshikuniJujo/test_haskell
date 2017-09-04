{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Reader (Reader, runReader, runReader2, ask) where

import Control.Monad.Cont
import Data.Typeable

import MyEff

newtype Reader e v = Reader (e -> v)
	deriving (Typeable, Functor)

ask :: (Member (Reader e) r, Typeable e) => Cont (VE r a) e
ask = cont $ toEffect . Reader

runReader :: (Typeable r, Typeable e) =>
	Cont (VE (Reader e :> r) a) a -> e -> VE r a
runReader m = rloop (runCont m V)

runReader2 :: (Typeable r, Typeable e) =>
	Cont (VE (Reader e :> r) a) a -> e -> Cont (VE r a) a
runReader2 m = cont . const . runReader m

rloop :: (Typeable r, Typeable e) => VE (Reader e :> r) a -> e -> VE r a
rloop m e = case m of
	V x -> V x
	E u -> case fromEffect m of
		Just (Reader r) -> rloop (r e) e
		Nothing -> E $ fmap (`rloop` e) u
