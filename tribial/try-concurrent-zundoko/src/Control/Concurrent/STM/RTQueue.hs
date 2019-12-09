{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.STM.RTQueue (
	TRTQueue, newqueue, enqueue, dequeue, requeue ) where

import Control.Concurrent.STM (
	STM, retry, TVar, newTVar, readTVar, writeTVar, modifyTVar )
import Data.RTQueue (RTQueue, empty, snoc, uncons, cons)

type TRTQueue a = TVar (RTQueue a)

newqueue :: STM (TRTQueue a)
newqueue = newTVar empty

enqueue :: TRTQueue a -> a -> STM ()
enqueue q = modifyTVar q . flip snoc

dequeue :: TRTQueue a -> STM a
dequeue q = uncons <$> readTVar q >>= \case
	Nothing -> retry
	Just (x, xs) -> x <$ writeTVar q xs

requeue :: TRTQueue a -> a -> STM ()
requeue q = modifyTVar q . cons
