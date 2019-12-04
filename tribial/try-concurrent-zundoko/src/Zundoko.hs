{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Zundoko (ToEndable(..), zundoko) where

import Control.Arrow (first)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Tips (forkForever)
import Control.Concurrent.STM (
	atomically, retry, TVar, newTVar, readTVar, writeTVar )
import Control.Concurrent.STM.RTQueue (newqueue, enqueue)
import Data.List.KnuthMorrisPratt (KmpState, initialState, nextState, found)
import Data.RTQueue (RTQueue, empty, uncons)
import System.Random (randomRIO)

class ToEndable a where
	type PreEndable a
	endable :: PreEndable a -> a; endValue :: a

	default endable :: PreEndable a ~ a => PreEndable a -> a; endable = id

instance ToEndable (Maybe a) where
	type PreEndable (Maybe a) = a
	endable = Just; endValue = Nothing

zundoko :: forall e . (ToEndable e, Eq (PreEndable e)) =>
	[PreEndable e] -> [PreEndable e] -> IO (TVar (RTQueue e))
zundoko ts pt = do
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
				(enqueue qo . endable) `mapM_` zs
				maybe (enqueue qo (endValue :: e)) (writeTVar kmpst) st'
	pure qo

ruffle :: Int -> IO ()
ruffle n = randomRIO (1, n) >>= threadDelay

check :: Eq a => KmpState a -> RTQueue a -> ([a], Maybe (KmpState a))
check st q = case uncons q of
	Nothing -> ([], Just st)
	Just (z, q') -> let st' = st `nextState` z in
		if found st' then ([z], Nothing) else (z :) `first` check st' q'
