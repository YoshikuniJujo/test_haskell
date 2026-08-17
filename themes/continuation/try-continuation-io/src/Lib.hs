{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Maybe
import Data.IORef
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Cont

someFunc :: IO ()
someFunc = putStrLn "someFunc"

f :: Int -> String
f x = evalCont $ callCC $ \ret -> do
	when (x == 0) (ret "zero")
	return "non-zero"

action :: IO ()
action = do
	print $ f 0
	print $ f 1

foo :: ContT r IO ()
foo = do
	r <- lift $ newIORef Nothing
	xr <- lift $ newIORef (0 :: Int)
	lift $ putStrLn "begin"
	(lift . print =<<) . callCC $ \ret -> do
		lift $ writeIORef r $ Just ret
		lift do	putStrLn "get ret"
			putStrLn "end"
		return 1234
	lift $ putStrLn "after ret"
	lift $ modifyIORef xr (+ 1)
	x <- lift $ readIORef xr
	lift $ print x
	when (x < 10)
		$ ($ x * 100) . fromMaybe (const $ pure ()) =<< lift (readIORef r)
