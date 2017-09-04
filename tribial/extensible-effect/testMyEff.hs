{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import MyEff

some :: Cont (VE (State Integer :> Lift IO :> ()) ()) ()
some = do
	_ <- modify (const 88 :: Integer -> Integer)
	(x :: Integer) <- get
	lift $ print x

data NotTypeable = NT deriving Show

other :: Cont (VE (State Integer :> ()) NotTypeable) NotTypeable
other = return $ NT

hoge :: Cont (VE (State NotTypeable :> ()) Integer) Integer
hoge = return 8888888
