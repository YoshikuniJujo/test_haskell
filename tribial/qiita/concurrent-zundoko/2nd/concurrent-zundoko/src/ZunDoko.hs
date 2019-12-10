{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies, DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ZunDoko (ToEndable(..), zundoko) where

import Control.Arrow
import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import System.Random

import Tips
import RealtimeQueue
import RealtimeQueueStm
import KnuthMorrisPratt

class ToEndable a where
	type PreEndable a
	endable :: PreEndable a -> a
	endValue :: a

	default endable :: PreEndable a ~ a => PreEndable a -> a
	endable = id

instance ToEndable (Maybe a) where
	type PreEndable (Maybe a) = a
	endable = Just
	endValue = Nothing

ruffle :: Int -> IO ()
ruffle n = randomRIO (1, n) >>= threadDelay

check :: Eq a => KmpState a -> RTQueue a -> ([a], Maybe (KmpState a))
check st q = case uncons q of
	Nothing -> ([], Just st)
	Just (z, q') -> let st' = st `nextState` z in
		if found st' then ([z], Nothing) else (z :) `first` check st' q'

zundoko :: (ToEndable e, Eq (PreEndable e)) =>
	[PreEndable e] -> [PreEndable e] -> IO (TVar (RTQueue e))

zundoko ts pt = do
	ql <- atomically newQueue
	(forkForever . (ruffle 100000 >>) . atomically . enqueue ql) `mapM_` ts
	kmpst <- atomically . newTVar $ initialState pt
	qo <- atomically newQueue
	forkLoopIf . atomically $ do
		st <- readTVar kmpst
		q <- readTVar ql; writeTVar ql empty
		case check st q of
			([], _) -> retry
			(zs, st') -> (enqueue qo . endable) `mapM_` zs >> maybe
				(False <$ enqueue qo endValue)
				((True <$) . writeTVar kmpst) st'
	pure qo
