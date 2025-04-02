{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module FreerMonads where

import Control.Arrow
import Freer

data Reader e a where Ask :: Reader e e

ask :: Freer (Reader e) e
ask = freer Ask

runReader :: Freer (Reader e) a -> e -> a
runReader m e = case m of Pure x -> x; Ask `Bind` k -> runReader (k e) e

data Writer w a where Tell :: w -> Writer w ()

tell :: w -> Freer (Writer w) ()
tell = freer . Tell

runWriter :: Monoid w => Freer (Writer w) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Tell w `Bind` k -> ((w <>) `second`) . runWriter $ k ()

data State s a where Get :: State s s; Put :: s -> State s ()

get :: Freer (State s) s
get = freer Get

put :: s -> Freer (State s) ()
put = freer . Put

modify :: (s -> s) -> Freer (State s) ()
modify f = put . f =<< get

runState :: Freer (State s) a -> s -> (a, s)
runState m s = case m of
	Pure x -> (x, s)
	Get `Bind` k -> runState (k s) s
	Put s' `Bind` k -> runState (k ()) s'

newtype Exc e a = ThrowError e

throwError :: e -> Freer (Exc e) a
throwError = freer . ThrowError

runError :: Freer (Exc e) a -> Either e a
runError = \case Pure x -> Right x; ThrowError e `Bind` _ -> Left e

catchError :: Freer (Exc e) a -> (e -> Freer (Exc e) a) -> Freer (Exc e) a
m `catchError` h = case m of Pure x -> pure x; Bind (ThrowError e) _k -> h e
