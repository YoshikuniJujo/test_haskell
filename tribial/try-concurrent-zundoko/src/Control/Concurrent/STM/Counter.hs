{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Concurrent.STM.Counter (
	Counter, newCounter, countup, countdown ) where

import Control.Concurrent.STM (STM, retry, TVar, newTVar, readTVar, writeTVar)

data Counter = Counter Int (TVar Int)

newCounter :: Int -> STM Counter
newCounter n = Counter n <$> newTVar 0

countup, countdown :: Counter -> STM ()
countup (Counter n cnt) = readTVar cnt >>= \c ->
	if c >= n then retry else writeTVar cnt $ c + 1

countdown (Counter _ cnt) = readTVar cnt >>= \c ->
	if c <= 0 then retry else writeTVar cnt $ c - 1
