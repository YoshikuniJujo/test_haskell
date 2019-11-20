{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head)

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM

data Queue a = Queue (List a) (TVar (List a))

type List a = TVar (Cons a)
data Cons a = Nil | Cons a (List a)

newQueue :: STM (Queue a)
newQueue = do
	e <- newTVar Nil
	l <- newTVar e
	pure $ Queue e l

snoc :: Queue a -> a -> STM ()
snoc (Queue _ end) x = do
	e <- readTVar end
	new <- newTVar Nil
	writeTVar e $ Cons x new
	writeTVar end new

head :: Queue a -> STM a
head (Queue lst _) = readTVar lst >>= \case
	Nil -> retry
	Cons x t -> x <$ (writeTVar lst =<< readTVar t)

try :: IO ()
try = do
	q <- atomically newQueue
	atomically $ (q `snoc`) `mapM_` [1 :: Int .. 10]
	atomically (replicateM 10 $ head q) >>= print
