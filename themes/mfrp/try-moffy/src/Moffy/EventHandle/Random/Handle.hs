{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.EventHandle.Random.Handle (
	-- * Type
	RandomEv, RandomState(..),
	-- * Handle
	handleRandom ) where

import Control.Monad.State (StateT, gets, modify)
import Data.Type.Set (Singleton)
import Data.OneOrMore (singleton, extract)
import System.Random (StdGen)

import Control.Moffy.Handle
import Moffy.EventHandle.Random.Event

---------------------------------------------------------------------------

-- * EVENT
--	+ STORE RANDOM GEN
--	+ LOAD RANDOM GEN
-- * REACT AND HANDLE
--	+ TYPE
--	+ GET RANDOM FUNCTION
--	+ HANDLE

-- HANDLE

class RandomState s where
	getRandomGen :: s -> StdGen; putRandomGen :: s -> StdGen -> s

instance RandomState StdGen where getRandomGen = id; putRandomGen = flip const

handleRandom :: (RandomState s, Monad m) => Handle' (StateT s m) RandomEv
handleRandom = handleStoreRandomGen `merge` handleLoadRandomGen

handleStoreRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton StoreRandomGen)
handleStoreRandomGen rqs =
	Just (singleton OccStoreRandomGen) <$ modify (`putRandomGen` g)
	where StoreRandomGenReq g = extract rqs

handleLoadRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton LoadRandomGen)
handleLoadRandomGen _rqs =
	gets $ Just . singleton . OccLoadRandomGen . getRandomGen
