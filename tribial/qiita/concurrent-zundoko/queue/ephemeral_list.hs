{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude hiding (head)

import Control.Concurrent.STM

type List a = TVar (ConsVar a)
type ConsVar a = TVar (Cons a)
data Cons a = Nil | Cons a (ConsVar a)

newList :: STM (List a)
newList = newTVar =<< newTVar Nil

cons :: a -> List a -> STM ()
cons x l = writeTVar l =<< newTVar . Cons x =<< readTVar l

head :: List a -> STM a
head l = (readTVar =<< readTVar l) >>= \case
	Nil -> retry
	Cons x t -> x <$ writeTVar l t

consAll :: [a] -> List a -> STM ()
consAll xs l = (`cons` l) `mapM_` xs

makeList :: [a] -> STM (List a)
makeList xs = (>>) <$> consAll xs <*> pure =<< newList

breakList :: List a -> STM [a]
breakList = (bl =<<) . readTVar
	where
	bl l = readTVar l >>= \case Nil -> pure []; Cons x t -> (x :) <$> bl t
