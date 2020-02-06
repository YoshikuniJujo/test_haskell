{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM
import System.IO.Unsafe

globalHello :: TVar String
globalHello = unsafePerformIO . atomically $ newTVar "Hello, world!"

globalGoodbye :: TVar String
globalGoodbye = unsafePerformIO . atomically $ newTVar "Good-bye, world!"

hello :: String
hello = unsafePerformIO $ readTVarIO globalHello

goodbye :: String
goodbye = unsafePerformIO . atomically $ readTVar globalGoodbye

main :: IO ()
main = do
--	putStrLn hello
	putStrLn goodbye

main2 :: IO ()
main2 = do
	putStrLn hello
	putStrLn goodbye
