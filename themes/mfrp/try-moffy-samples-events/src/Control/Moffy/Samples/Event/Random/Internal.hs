{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Event.Random.Internal (
	-- * Store Random Gen
	StoreRandomGen(..), pattern OccStoreRandomGen,
	-- * Load Random Gen
	LoadRandomGen, pattern OccLoadRandomGen,
	-- * Get Random
	RandomEv, getRandom, getRandomR ) where

import Control.Arrow ((>>>))
import Control.Moffy (React, Request(..), await, adjust)
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-))
import Data.OneOrMore (Selectable(..))
import System.Random (Random, StdGen, random, randomR)

---------------------------------------------------------------------------

-- * STORE RANDOM GEN
-- * LOAD RANDOM GEN
-- * RANDOM EV AND GET RANDOM

---------------------------------------------------------------------------
-- STORE RANDOM GEN
---------------------------------------------------------------------------

newtype StoreRandomGen = StoreRandomGenReq StdGen deriving Show
numbered [t| StoreRandomGen |]
instance Selectable StoreRandomGen where l `select` _r = l
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen

storeRandomGen :: StdGen -> React s (Singleton StoreRandomGen) ()
storeRandomGen g = await (StoreRandomGenReq g) \OccStoreRandomGen -> ()

---------------------------------------------------------------------------
-- LOAD RANDOM GEN
---------------------------------------------------------------------------

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGen

loadRandomGen :: React s (Singleton LoadRandomGen) StdGen
loadRandomGen = await LoadRandomGenReq \(OccLoadRandomGen g) -> g

---------------------------------------------------------------------------
-- RANDOM EV AND GET RANDOM
---------------------------------------------------------------------------

type RandomEv = StoreRandomGen :- LoadRandomGen :- 'Nil

getRandom :: Random a => React s RandomEv a
getRandom = modifyRandomGen random

getRandomR :: Random a => (a, a) -> React s RandomEv a
getRandomR = modifyRandomGen . randomR

modifyRandomGen :: (StdGen -> (a, StdGen)) -> React s RandomEv a
modifyRandomGen f = adjust loadRandomGen
	>>= (f >>> \(r, g) -> r <$ adjust (storeRandomGen g))
