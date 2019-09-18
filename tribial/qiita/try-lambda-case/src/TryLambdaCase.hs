{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryLambdaCase where

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

data Ping = Ping (TChan Pong) | Finished

data Pong = Pong

ping :: TChan Pong -> Int -> TChan Ping -> IO ()
ping _self n pon | n < 1 = do
	atomically $ writeTChan pon Finished
	putStrLn "ping finished"
ping self n pon = do
	atomically $ writeTChan pon (Ping self)
	atomically (readTChan self) >>= \case
		Pong -> putStrLn "Ping received pong"
	ping self (n - 1) pon

pong :: TChan Ping -> IO ()
pong self = atomically (readTChan self) >>= \case
	Finished -> putStrLn "Pong finished"
	Ping pin -> do
		putStrLn "Pong received ping"
		atomically $ writeTChan pin Pong
		pong self

run :: IO ()
run = do
	pin <- newTChanIO
	pon <- newTChanIO
	_ <- forkIO $ pong pon
	ping pin 3 pon
