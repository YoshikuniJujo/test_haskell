{-# LANGUAGE LambdaCase, DeriveFunctor #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid

import Free

data Reader r w = Reader (r -> w) deriving Functor

ask :: Free (Reader r) r
ask = Free $ Reader Pure

runReader :: Free (Reader r) a -> r -> a
runReader f r = case f of
	Pure x -> x
	Free (Reader f') -> runReader (f' r) r

data Writer wr w = Writer wr w deriving Functor

tell :: wr -> Free (Writer wr) ()
tell wr = Free $ Writer wr (Pure ())

runWriter :: Monoid wr => Free (Writer wr) a -> (a, wr)
runWriter f = case f of
	Pure x -> (x, mempty)
	Free (Writer wr f') -> second (wr <>) $ runWriter f'

data State s w = State (s -> s) (s -> w) deriving Functor

getModify :: (s -> s) -> Free (State s) s
getModify f = Free $ State f Pure

get :: Free (State s) s
get = getModify id

modify :: (s -> s) -> Free (State s) ()
modify = (>> return ()) . getModify

put :: s -> Free (State s) ()
put = modify . const

runState :: Free (State s) a -> s -> (a, s)
runState f s = case f of
	Pure x -> (x, s)
	Free (State u f') -> runState (f' s) (u s)

data RW r wr w = R (r -> w) | W wr w deriving Functor

rwAsk :: Free (RW r wr) r
rwAsk = Free $ R Pure

rwTell :: wr -> Free (RW r wr) ()
rwTell wr = Free $ W wr (Pure ())

runRW :: Monoid wr => Free (RW r wr) a -> r -> (a, wr)
runRW f r = case f of
	Pure x -> (x, mempty)
	Free (R f') -> runRW (f' r) r
	Free (W wr f') -> second (wr <>) $ runRW f' r
