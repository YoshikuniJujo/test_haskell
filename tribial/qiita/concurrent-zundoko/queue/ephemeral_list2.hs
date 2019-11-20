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

consAll :: [a] -> List a -> STM ()
consAll xs l = (`cons` l) `mapM_` xs

makeList :: [a] -> STM (List a)
makeList xs = (>>) <$> consAll xs <*> pure =<< newList

breakList :: List a -> STM [a]
breakList l =
	readTVar l >>= \case Nil -> pure []; Cons x t -> (x :) <$> breakList t

snoc :: List a -> a -> STM ()
snoc l x = readTVar l >>= \case
	Nil -> writeTVar l . Cons x =<< newTVar Nil
	Cons _ t -> snoc t x

snocAll :: List a -> [a] -> STM ()
snocAll l xs = (l `snoc`) `mapM_` xs
