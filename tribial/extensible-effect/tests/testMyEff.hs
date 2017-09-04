{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Cont hiding (lift)

import Data.Void
import MyEff
import MyEff.Reader
import MyEff.State
import MyEff.Lift

some :: Cont (VE (State Integer :> Lift IO :> Void) ()) ()
some = do
	_ <- modify (const 88 :: Integer -> Integer)
	(x :: Integer) <- get
	lift $ print x

data NotTypeable = NT deriving Show

other :: Cont (VE (State Integer :> Void) NotTypeable) NotTypeable
other = return NT

hoge :: Cont (VE (State NotTypeable :> Void) Integer) Integer
hoge = return 8888888

piyo :: Cont (VE (State Integer :> Reader Char :> Void) (Integer, Char))
	(Integer, Char)
piyo = do
	x <- get
	y <- ask
	return (x, y)
