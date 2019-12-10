{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RealtimeQueueStm (TRTQueue, newQueue, enqueue, dequeue) where

import Control.Concurrent.STM
import RealtimeQueue

type TRTQueue a = TVar (RTQueue a)

newQueue :: STM (TRTQueue a)
newQueue = newTVar empty

enqueue :: TRTQueue a -> a -> STM ()
enqueue q = modifyTVar q . flip snoc

dequeue :: TRTQueue a -> STM a
dequeue q = uncons <$> readTVar q >>= \case
	Nothing -> retry
	Just (x, xs) -> x <$ writeTVar q xs
