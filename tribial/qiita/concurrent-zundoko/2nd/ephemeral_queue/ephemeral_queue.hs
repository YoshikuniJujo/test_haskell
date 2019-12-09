{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head)

import Control.Concurrent.STM

data Queue a = Queue (List a) (TVar (List a))

type List a = TVar (Cons a)
data Cons a = Nil | Cons a (List a)

newQueue :: STM (Queue a)
newQueue = newTVar Nil >>= \l -> Queue l <$> newTVar l

snoc :: Queue a -> a -> STM ()
snoc (Queue _ end) x = newTVar Nil >>= \new -> do
	(`writeTVar` Cons x new) =<< readTVar end
	writeTVar end new

head :: Queue a -> STM a
head (Queue lst _) = readTVar lst >>= \case
	Nil -> retry
	Cons x t -> x <$ (writeTVar lst =<< readTVar t)
