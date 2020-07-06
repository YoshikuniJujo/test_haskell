{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Random (
	-- * Type
	RandomEv, StoreRandomGen(..), LoadRandomGen,
	Occurred(OccStoreRandomGen, OccLoadRandomGen),
	-- * Event
	getRandom, getRandomR ) where

import Data.Type.Set (Set(Nil), Singleton, numbered, (:-))
import Data.OneOrMore (Selectable(..))
import System.Random (Random, StdGen, random, randomR)

import Control.Moffy

---------------------------------------------------------------------------

-- * EVENT
--	+ STORE RANDOM GEN
--	+ LOAD RANDOM GEN
-- * REACT AND HANDLE
--	+ TYPE
--	+ GET RANDOM FUNCTION
--	+ HANDLE

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- STORE RANDOM GEN

newtype StoreRandomGen = StoreRandomGenReq StdGen deriving Show
numbered 9 [t| StoreRandomGen |]
instance Selectable StoreRandomGen where gl `select` _gr = gl
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen

{-# ANN storeRandomGen "HLint: ignore Use const" #-}

storeRandomGen :: StdGen -> React s (Singleton StoreRandomGen) ()
storeRandomGen g = await (StoreRandomGenReq g) \_ -> ()

-- LOAD RANDOM GEN

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered 9 [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGen deriving Show

loadRandomGen :: React s (Singleton LoadRandomGen) StdGen
loadRandomGen = await LoadRandomGenReq \(OccLoadRandomGen g) -> g

---------------------------------------------------------------------------
-- REACT AND HANDKLE
---------------------------------------------------------------------------

-- TYPE

type RandomEv = StoreRandomGen :- LoadRandomGen :- 'Nil

-- GET RANDOM FUNCTION

getRandom :: Random a => React s RandomEv a
getRandom = modifyRandomGen random

getRandomR :: Random a => (a, a) -> React s RandomEv a
getRandomR = modifyRandomGen . randomR

modifyRandomGen :: (StdGen -> (a, StdGen)) -> React s RandomEv a
modifyRandomGen f = (f <$> adjust loadRandomGen) >>= \(r, g') ->
	r <$ adjust (storeRandomGen g')
