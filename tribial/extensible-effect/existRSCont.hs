{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont
import Data.Typeable

data VE a
	= Val a
	| forall t . (Functor t, Typeable t, Typeable a) => E (t (VE a))
	deriving Typeable

class (Functor e, Typeable e) => Effect e where
	toEffect :: Typeable a => e (VE a) -> VE a
	fromEffect :: VE a -> Maybe (e (VE a))

	toEffect = E
	fromEffect = \case
		E e -> cast e
		Val _ -> Nothing

newtype Reader e v = Reader (e -> v)
	deriving (Typeable, Functor)

instance Typeable e => Effect (Reader e) where

ask :: (Typeable e, Typeable a) => Cont (VE a) e
ask = cont $ toEffect . Reader

runReader :: Typeable e => Cont (VE a) a -> e -> VE a
runReader m = rloop (runCont m Val)

runReader2 :: Typeable e => Cont (VE a) a -> e -> Cont (VE a) a
runReader2 m = cont . const . runReader m

rloop :: Typeable e => VE a -> e -> VE a
rloop m e = case m of
	Val x -> Val x
	E u -> case fromEffect m of
		Just (Reader r) -> rloop (r e) e
		Nothing -> E $ fmap (`rloop` e) u

run :: VE a -> a
run (Val x) = x
run _ = undefined

run2 :: Cont (VE a) a -> a
run2 m = case runCont m Val of
	Val x -> x
	_ -> undefined

data State s a = State (s -> s) (s -> a)
	deriving (Typeable, Functor)

instance Typeable s => Effect (State s) where

modify :: (Typeable s, Typeable a) => (s -> s) -> Cont (VE a) s
modify f = cont $ toEffect . State f

get :: (Typeable s, Typeable a) => Cont (VE a) s
get = cont $ toEffect . State id

runState :: Typeable s => Cont (VE a) a -> s -> VE a
runState m = sloop (runCont m Val)

runState2 :: Typeable s => Cont (VE a) a -> s -> Cont (VE a) a
runState2 m = cont . const . runState m

sloop :: Typeable s => VE a -> s -> VE a
sloop m s = case m of
	Val x -> Val x
	E u -> case fromEffect m of
		Just (State f k) -> sloop (k s) (f s)
		Nothing -> E $ fmap (`sloop` s) u
