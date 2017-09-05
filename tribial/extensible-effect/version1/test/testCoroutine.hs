{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import MyEff
import MyEff.Lift
import MyEff.Coroutine
import TypeLevel

testCoroutine :: Eff (Yield Integer :> Lift IO :> Base) Bool
testCoroutine = do
	lift $ putStrLn "begin test"
	lift $ putStrLn "push 1"
	yield (1 :: Integer)
	lift $ putStrLn "push 4"
	yield (4 :: Integer)
	lift $ putStrLn "finished"
	return True
