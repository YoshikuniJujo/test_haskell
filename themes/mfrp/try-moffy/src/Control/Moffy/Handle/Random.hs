{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Random (
	-- * Type
	RandomEv, RandomState(..),
	-- * Handle
	handleRandom ) where

import Control.Moffy.Event.Random.Internal (
	RandomEv, StoreRandomGen(..), pattern OccStoreRandomGen,
	LoadRandomGen, pattern OccLoadRandomGen )
import Control.Moffy.Handle (HandleSt', mergeSt)
import Data.Type.Set (Singleton)
import Data.OneOrMore (pattern Singleton)
import System.Random (StdGen)

import Data.OneOrMoreApp

---------------------------------------------------------------------------

-- * RANDOM STATE
-- * HANDLE

---------------------------------------------------------------------------
-- RANDOM STATE
---------------------------------------------------------------------------

class RandomState s where
	getRandomGen :: s -> StdGen; putRandomGen :: s -> StdGen -> s

instance RandomState StdGen where getRandomGen = id; putRandomGen = flip const

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

handleRandom :: (RandomState s, Monad m) => HandleSt' s m RandomEv
handleRandom = handleStoreRandomGen `mergeSt` handleLoadRandomGen

handleStoreRandomGen :: (RandomState s, Applicative m) =>
	HandleSt' s m (Singleton StoreRandomGen)
handleStoreRandomGen (Singleton (StoreRandomGenReq g)) s =
	pure (Just $ SingletonApp OccStoreRandomGen, s `putRandomGen` g)

handleLoadRandomGen :: (RandomState s, Applicative m) =>
	HandleSt' s m (Singleton LoadRandomGen)
handleLoadRandomGen _rqs s =
	pure (Just . SingletonApp . OccLoadRandomGen $ getRandomGen s, s)
