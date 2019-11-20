{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad (void, forever)
import Control.Monad.Tips (loopIf)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (
	atomically, retry, newTVar, readTVar, writeTVar, modifyTVar )
import Control.Concurrent.STM.RTQueue (enqueue, dequeue, requeue)
import Control.Concurrent.STM.Counter (newCounter, countup, countdown)
import Data.List (foldl')
import Data.RTQueue (RTQueue, empty, snoc, uncons)
import System.Random (randomRIO)

data ZunDoko = Zun | Doko deriving Show
data Result
	= Pending
	| UntimelyDoko [Say] (RTQueue ZunDoko)
	| ZunOverflow (RTQueue ZunDoko)
	| Kiyoshi [Say] (RTQueue ZunDoko)
	deriving Show
data Say = SayZun | SayDoko | SayKiyoshi | Timeout deriving Show

say :: Say -> String
say SayZun = "ズン"
say SayDoko = "ドコ"
say SayKiyoshi = "キ・ヨ・シ!"
say Timeout = "<タイムアウト>"

main :: IO ()
main = do
	qz <- atomically $ newTVar empty
	qs <- atomically $ newTVar empty
	cnt <- atomically $ newCounter 1000
	void . forkIO . forever
		$ ruffle >> atomically (countup cnt >> enqueue qz Zun)
	void . forkIO . forever
		$ ruffle >> atomically (countup cnt >> enqueue qz Doko)
	void . forkIO $ delay 200000 >> atomically (requeue qs Timeout)
	void . forkIO . forever . atomically $ check 0 <$> readTVar qz >>= \case
		Pending -> retry
		UntimelyDoko zds q ->
			writeTVar qz q >> modifyTVar qs (flip (foldl' snoc) zds)
		ZunOverflow q ->
			writeTVar qz q >> modifyTVar qs (`snoc` SayZun)
		Kiyoshi zds q ->
			writeTVar qz q >> modifyTVar qs (flip (foldl' snoc) zds)
	loopIf $ do
		s <- atomically $ countdown cnt >> dequeue qs
		putStrLn $ say s
		delay 1000
		return $ case s of
			SayZun -> True; SayDoko -> True
			SayKiyoshi -> False; Timeout -> False

ruffle :: IO ()
ruffle = randomRIO (1, 1000) >>= delay

delay :: Int -> IO ()
delay = threadDelay . (* 100)

check :: Int -> RTQueue ZunDoko -> Result
check n q = case uncons q of
	Nothing -> Pending
	Just (Zun, q')
		| n < 4 -> case check (n + 1) q' of
			Pending -> Pending
			UntimelyDoko s q'' -> UntimelyDoko (SayZun : s) q''
			ZunOverflow _q'' -> ZunOverflow q'
			Kiyoshi s q'' -> Kiyoshi (SayZun : s) q''
		| otherwise -> ZunOverflow q'
	Just (Doko, q')
		| n < 4 -> UntimelyDoko [SayDoko] q'
		| otherwise -> Kiyoshi [SayDoko, SayKiyoshi] q'
