{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LoopAndException where

import Control.Concurrent
import Control.Exception
import Data.IORef

newtype Greeting = Greeting String deriving Show

greet :: Greeting -> IO ()
greet (Greeting str) = putStrLn str

run :: IO ()
run = do
	gr <- newIORef $ Greeting "Hello"
	ts <- newIORef $ replicate 10 ()
	loop ts gr

loop :: IORef [()] -> IORef Greeting -> IO ()
loop ts gr = do
	catch	do	threadDelay 1000000
			greet =<< readIORef gr
			print =<< readIORef ts
			modifyIORef ts tail
		\(_ :: ErrorCall) -> do
			writeIORef gr $ Greeting "Good-bye"
			writeIORef ts $ replicate 10 ()
	loop ts gr

run' :: IO ()
run' = loop' (replicate 10 ()) (Greeting "Hello")

loop' :: [()] -> Greeting -> IO ()
loop' ts gr = do
	catch	do	threadDelay 1000000
			greet gr
			print ts
			loop' (tail ts) gr
		\(_ :: ErrorCall) ->
			loop' (replicate 10 ()) $ Greeting "Good-bye"
