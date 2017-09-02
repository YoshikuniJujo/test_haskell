{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont

data VE e s a
	= Val a
	| E (Reader e (VE e s a))
	| S (State s (VE e s a))

newtype Reader e v = Reader (e -> v)

ask :: Cont (VE e s a) e
ask = cont $ E . Reader

runReader :: Cont (VE e s a) a -> e -> a
runReader m e = rloop (runCont m Val) e

rloop :: VE e s a -> e -> a
rloop m e = case m of
	Val x -> x
	E (Reader u) -> rloop (u e) e
	_ -> undefined

data State s w = State (s -> s) (s -> w)

modify :: (s -> s) -> Cont (VE e s a) s
modify f = cont $ S . State f

get :: Cont (VE e s a) s
get = cont $ S . State id

runState :: Cont (VE e s a) a -> s -> a
runState m s = sloop (runCont m Val) s

sloop :: VE e s a -> s -> a
sloop m s = case m of
	Val x -> x
	S (State f k) -> sloop (k s) (f s)
	_ -> undefined

rsloop :: VE e s a -> e -> s -> a
rsloop m e s = case m of
	Val x -> x
	E (Reader u) -> rsloop (u e) e s
	S (State f k) -> rsloop (k s) e (f s)

runRS :: Cont (VE e s a) a -> e -> s -> a
runRS m e s = rsloop (runCont m Val) e s
