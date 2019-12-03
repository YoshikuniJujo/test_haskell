{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Tips
import Control.Concurrent
import Control.Concurrent.STM (atomically, retry, newTVar, readTVar, writeTVar)
import Control.Concurrent.STM.RTQueue
import Data.List.KnuthMorrisPratt
import Data.RTQueue
import System.Random

data ZunDoko = Zun | Doko | End deriving (Show, Eq)

main :: IO ()
main = do
	qz <- atomically newqueue
	qs <- atomically newqueue
	st <- atomically . newTVar $ initialState [Zun, Zun, Zun, Zun, Doko]
	void . forkIO . forever $ ruffle 100000 >> atomically (enqueue qz Zun)
	void . forkIO . forever $ ruffle 100000 >> atomically (enqueue qz Doko)
	void . forkIO . forever .  atomically $ do
		s <- readTVar st
		q <- readTVar qz
		case check s q of
			([], _) -> retry
			(zds, s') -> do
				enqueue qs `mapM_` zds
				case s' of
					Nothing -> enqueue qs End
					Just ss' -> writeTVar st ss'
				writeTVar qz empty
	loopIf $ do
		s <- atomically $ dequeue qs
		print s
		return $ s /= End
		

ruffle :: Int -> IO ()
ruffle n = randomRIO (1, n) >>= threadDelay

check :: Eq a => KmpState a -> RTQueue a -> ([a], Maybe (KmpState a))
check st q = case uncons q of
	Nothing -> ([], Just st)
	Just (zd, q') -> let st' = nextState st zd in
		if found st' then ([zd], Nothing) else let
			(zds, st'') = check st' q' in
			(zd : zds, st'')
