{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import MyEff

some :: Cont (VE (State Integer :> Lift IO :> ()) ()) ()
some = do
	_ <- modify (const 88 :: Integer -> Integer)
	(x :: Integer) <- get
	lift $ print x
