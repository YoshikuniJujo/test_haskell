{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Zundoko (zundoko) where

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Tips (forkForever)
import Control.Concurrent.STM (
	atomically, retry, TVar, newTVar, readTVar, writeTVar )
import Control.Concurrent.STM.RTQueue (newqueue, enqueue)
import Data.List.KnuthMorrisPratt (KmpState, initialState, nextState, found)
import Data.RTQueue (RTQueue, empty, uncons)
import System.Random (randomRIO)

zundoko :: Eq a => [a] -> [a] -> a -> IO (TVar (RTQueue a))
zundoko ts pt end = do
	ql <- atomically newqueue
	(forkForever . (ruffle 100000 >>) . atomically . enqueue ql) `mapM_` ts
	kmpst <- atomically . newTVar $ initialState pt
	qo <- atomically newqueue
	forkForever . atomically $ do
		st <- readTVar kmpst
		q <- readTVar ql; writeTVar ql empty
		case check st q of
			([], _) -> retry
			(zs, st') -> do
				enqueue qo `mapM_` zs
				maybe (enqueue qo end) (writeTVar kmpst) st'
	pure qo

ruffle :: Int -> IO ()
ruffle n = randomRIO (1, n) >>= threadDelay

check :: Eq a => KmpState a -> RTQueue a -> ([a], Maybe (KmpState a))
check st q = case uncons q of
	Nothing -> ([], Just st)
	Just (z, q') -> let st' = nextState st z in
		if found st' then ([z], Nothing) else (z :) `first` check st' q'
