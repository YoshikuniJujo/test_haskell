{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head)

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM

data Queue a = Queue (TVar (List a)) (TVar (List a))

type List a = TVar (Cons a)
data Cons a = Nil | Cons a (List a)

newQueue :: STM (Queue a)
newQueue = do
	e <- newTVar Nil
	h <- newTVar e
	l <- newTVar e
	pure $ Queue h l

snoc :: Queue a -> a -> STM ()
snoc (Queue _ l) x = do
	lst <- readTVar l
	new <- newTVar Nil
	writeTVar lst $ Cons x new
	writeTVar l new

head :: Queue a -> STM a
head (Queue h _) = (readTVar =<< readTVar h) >>= \case
	Nil -> retry
	Cons x t -> x <$ writeTVar h t

try :: IO ()
try = do
	q <- atomically newQueue
	atomically $ (q `snoc`) `mapM_` [1 :: Int .. 10]
	atomically (replicateM 10 $ head q) >>= print
