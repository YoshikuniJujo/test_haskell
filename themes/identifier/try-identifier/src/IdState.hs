{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IdState (Id, newId, sample) where

import Control.Monad.State

newtype Id = Id Integer deriving Show

newId :: State Integer Id
newId = do
	i <- get
	Id i <$ put (i + 1)

sample :: State Integer [Id]
sample = replicateM 10 newId
