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

import Control.Moffy.Event.Random.Internal (
	RandomEv, StoreRandomGen(..), pattern OccStoreRandomGen,
	LoadRandomGen, pattern OccLoadRandomGen )
import Control.Moffy.Handle (HandleIo', mergeSt)
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
-- NEW HANDLE
---------------------------------------------------------------------------

handleRandom :: (RandomState s, Monad m) => HandleIo' s s m RandomEv
handleRandom = handleStoreRandomGen' `mergeSt` handleLoadRandomGen'

handleStoreRandomGen' :: (Applicative m, RandomState s) =>
	HandleIo' s s m (Singleton StoreRandomGen)
handleStoreRandomGen' (Singleton (StoreRandomGenReq g)) s =
	pure (Just $ Singleton OccStoreRandomGen, s `putRandomGen` g)

handleLoadRandomGen' :: (Applicative m, RandomState s) =>
	HandleIo' s s m (Singleton LoadRandomGen)
handleLoadRandomGen' _rqs s =
	pure (Just . Singleton . OccLoadRandomGen $ getRandomGen s, s)
