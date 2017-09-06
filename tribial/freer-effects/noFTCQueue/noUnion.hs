{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Arrow
import Data.Monoid

import Freer

data Reader e a where
	Reader :: Reader e e

ask :: Freer (Reader e) e
ask = Join Reader Pure

runReader :: Freer (Reader e) a -> e -> a
runReader m e = case m of
	Pure x -> x
	Join Reader q -> runReader (q e) e

data Writer w a where
	Writer :: w -> Writer w ()

tell :: w -> Freer (Writer w) ()
tell w = Join (Writer w) Pure

runWriter :: Monoid w => Freer (Writer w) a -> (a, w)
runWriter = \case
	Pure x -> (x, mempty)
	Join (Writer w) k -> second (w <>) . runWriter $ k ()

data State s a where
	Get :: State s s
	Put :: s -> State s ()

get :: Freer (State s) s
get = Join Get Pure

put :: s -> Freer (State s) ()
put s =  Join (Put s) Pure

modify :: (s -> s) -> Freer (State s) ()
modify f = fmap f get >>= put

runState :: Freer (State s) a -> s -> (a, s)
runState m s = case m of
	Pure x -> (x, s)
	Join Get q -> runState (q s) s
	Join (Put s') q -> runState (q ()) s'
