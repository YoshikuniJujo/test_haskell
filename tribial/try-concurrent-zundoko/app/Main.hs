{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad (void, forever)
import Control.Monad.STM (STM, atomically, retry)
import Control.Monad.Tips (doWhile)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TVar (
	TVar, newTVar, readTVar, writeTVar, modifyTVar )
import Control.Exception.NeverOccur (neverOccur)
import Data.RTQueue (RTQueue, empty, snoc, uncons, cons)
import System.Random (randomRIO)

data ZunDoko = Zun | Doko | Quit deriving Show
data Kiyoshi = Timeout | Pending | Kiyoshi | NotKiyoshi (RTQueue ZunDoko)
	deriving Show

main :: IO ()
main = do
	zdv <- atomically $ newTVar empty
	void . forkIO . forever
		$ disturb >> putStrLn "ズン" >> atomically (enqueue zdv Zun)
	void . forkIO . forever
		$ disturb >> putStrLn "ドコ" >> atomically (enqueue zdv Doko)
	void . forkIO $ delay 50000 >> atomically (modifyTVar zdv $ cons Quit)
	k <- atomically . doWhile $ kiyoshiCheck 0 <$> readTVar zdv >>= \case
		Timeout -> pure $ Just False
		Pending -> retry
		Kiyoshi -> pure $ Just True
		NotKiyoshi zd' -> Nothing <$ writeTVar zdv zd'
	putStrLn $ if k then "キ・ヨ・シ!" else "<タイムアウト>"

enqueue :: TVar (RTQueue a) -> a -> STM ()
enqueue q x = modifyTVar q (`snoc` x)

disturb :: IO ()
disturb = randomRIO (1, 1000) >>= delay

delay :: Int -> IO ()
delay = threadDelay . (* 100)

kiyoshiCheck :: Int -> RTQueue ZunDoko -> Kiyoshi
kiyoshiCheck n _ | n < 0 = neverOccur
kiyoshiCheck n q = case uncons q of
	Nothing -> Pending
	Just (Quit, _) -> Timeout
	Just (Zun, zd) -> kiyoshiCheck (n + 1) zd
	Just (Doko, zd)
		| n > 3 -> Kiyoshi
		| otherwise -> NotKiyoshi zd
