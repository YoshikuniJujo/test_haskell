{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Random (
	-- * Type
	RandomEv, RandomState(..),
	-- * Handle
	handleRandom ) where

import Control.Monad.State (StateT, gets, modify)
import Control.Moffy.Handle (Handle', merge)
import Control.Moffy.Event.Random.Internal (
	RandomEv,
	StoreRandomGen(..), pattern OccStoreRandomGen,
	LoadRandomGen, pattern OccLoadRandomGen )
import Data.Type.Set (Singleton)
import Data.OneOrMore (pattern Singleton)
import System.Random (StdGen)

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

handleRandom :: (RandomState s, Monad m) => Handle' (StateT s m) RandomEv
handleRandom = handleStoreRandomGen `merge` handleLoadRandomGen

handleStoreRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton StoreRandomGen)
handleStoreRandomGen (Singleton (StoreRandomGenReq g)) =
	Just (Singleton OccStoreRandomGen) <$ modify (`putRandomGen` g)

handleLoadRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton LoadRandomGen)
handleLoadRandomGen _rqs =
	gets $ Just . Singleton . OccLoadRandomGen . getRandomGen
