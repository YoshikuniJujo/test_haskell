{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Random (
	-- * Type
	RandomEv,
	StoreRandomGen(..), pattern OccStoreRandomGen,
	LoadRandomGen, pattern OccLoadRandomGen,
	-- * GET RANDOM
	getRandom, getRandomR ) where

import Control.Moffy (React, Request(..), adjust, await)
import Data.Type.Set (numbered, Set(Nil), Singleton, (:-))
import Data.OneOrMore (Selectable(..))
import System.Random (Random, StdGen, random, randomR)

---------------------------------------------------------------------------

-- * EVENT
--	+ STORE RANDOM GEN
--	+ LOAD RANDOM GEN
-- * RANDOM EV AND GET RANDOM

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- STORE RANDOM GEN

newtype StoreRandomGen = StoreRandomGenReq StdGen deriving Show
numbered 64 [t| StoreRandomGen |]
instance Selectable StoreRandomGen where l `select` _r = l
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen

storeRandomGen :: StdGen -> React s (Singleton StoreRandomGen) ()
storeRandomGen g = await (StoreRandomGenReq g) \OccStoreRandomGen -> ()

-- LOAD RANDOM GEN

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered 64 [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGen deriving Show

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
modifyRandomGen f = (f <$> adjust loadRandomGen) >>= \(r, g') ->
	r <$ adjust (storeRandomGen g')
