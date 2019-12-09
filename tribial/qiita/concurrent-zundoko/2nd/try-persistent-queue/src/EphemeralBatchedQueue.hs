{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module EphemeralBatchedQueue (Q.BatchedQueue, newQueue, snoc, head) where

import Prelude hiding (head)

import Control.Concurrent.STM

import qualified BatchedQueue as Q

newQueue :: STM (TVar (Q.BatchedQueue a))
newQueue = newTVar Q.empty

snoc :: TVar (Q.BatchedQueue a) -> a -> STM ()
snoc q x = modifyTVar q (`Q.snoc` x)

head :: TVar (Q.BatchedQueue a) -> STM a
head q = Q.uncons <$> readTVar q >>= \case
	Nothing -> retry
	Just (x, t) -> x <$ writeTVar q t
