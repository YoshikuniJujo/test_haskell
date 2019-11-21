{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module EphemeralListQueue (P.ListQueue, newQueue, snoc, head) where

import Prelude hiding (head)

import Control.Concurrent.STM

import qualified ListQueue as P

newQueue :: STM (TVar (P.ListQueue a))
newQueue = newTVar P.empty

snoc :: TVar (P.ListQueue a) -> a -> STM ()
snoc q x = modifyTVar q (`P.snoc` x)

head :: TVar (P.ListQueue a) -> STM a
head q = P.uncons <$> readTVar q >>= \case
	Nothing -> retry
	Just (x, t) -> x <$ writeTVar q t
