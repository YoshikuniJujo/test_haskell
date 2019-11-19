{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head)

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM

type Stack a = TVar (List a)

type List a = TVar (Cons a)
data Cons a = Nil | Cons a (List a)

newStack :: STM (Stack a)
newStack = newTVar =<< newTVar Nil

cons :: a -> Stack a -> STM ()
cons x s = do
	lst <- readTVar s
	new <- newTVar $ Cons x lst
	writeTVar s new

head :: Stack a -> STM a
head s = do
	lst <- readTVar s
	readTVar lst >>= \case
		Nil -> retry
		Cons x lst' -> x <$ writeTVar s lst'

try :: IO ()
try = do
	st <- atomically newStack
	atomically $ (`cons` st) `mapM_` [1 :: Int .. 10]
	atomically (replicateM 10 $ head st) >>= print
