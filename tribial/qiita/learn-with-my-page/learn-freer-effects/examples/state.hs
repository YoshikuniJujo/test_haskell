{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Freer

data State s a where
	Get :: State s s
	Put :: s -> State s ()

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

sample :: Freer (State Integer) Integer
sample = do
	modify (+ 8)
	modify (* 5)
	s <- get
	put 4
	modify (+ 3)
	s' <- get
	return $ s + s'
