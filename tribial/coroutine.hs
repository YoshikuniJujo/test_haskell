{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Monad.Trans
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

producer :: Coroutine (Yield Int) IO String
producer = do
	yield 1
	lift (putStrLn "Produced one, next is four.")
	yield 4
	return "Finished"

printProduce :: Show x => Coroutine (Yield x) IO r -> IO r
printProduce prd = pogoStick
	(\(Yield x cont) -> lift (print x) >> cont)
	prd

some :: IO ()
some = do
	Left (Yield n c) <- resume producer
	putStr "Step 1: "
	print n
	Left (Yield n' c') <- resume c
	putStr "Step 2: "
	print n'
	Right r <- resume c'
	putStr "Step 3: "
	putStrLn r
