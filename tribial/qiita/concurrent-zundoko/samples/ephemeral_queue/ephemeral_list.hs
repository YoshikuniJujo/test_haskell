{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head)

import Control.Concurrent.STM

type List a = TVar (Cons a)
data Cons a = Nil | Cons a (List a)

newList :: STM (List a)
newList = newTVar Nil

cons :: a -> List a -> STM ()
cons x l = writeTVar l . Cons x =<< newTVar =<< readTVar l

head :: List a -> STM a
head l = readTVar l >>= \case
	Nil -> retry
	Cons x t -> x <$ (writeTVar l =<< readTVar t)

snoc :: List a -> a -> STM ()
snoc l x = readTVar l >>= \case
	Nil -> writeTVar l . Cons x =<< newTVar Nil
	Cons _ t -> snoc t x
