{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.STM.RTQueue (
	TRTQueue, enqueue, dequeue, requeue ) where

import Control.Concurrent.STM (
	STM, retry, TVar, readTVar, writeTVar, modifyTVar )
import Data.RTQueue (RTQueue, snoc, uncons, cons)

type TRTQueue a = TVar (RTQueue a)

enqueue :: TRTQueue a -> a -> STM ()
enqueue q = modifyTVar q . flip snoc

dequeue :: TRTQueue a -> STM a
dequeue q = uncons <$> readTVar q >>= \case
	Nothing -> retry
	Just (x, xs) -> x <$ writeTVar q xs

requeue :: TRTQueue a -> a -> STM ()
requeue q = modifyTVar q . cons
