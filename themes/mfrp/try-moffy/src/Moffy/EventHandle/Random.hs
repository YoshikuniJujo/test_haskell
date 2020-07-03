{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.EventHandle.Random (
	-- * Type
	RandomEv, RandomState(..),
	-- * Handle
	handleRandom,
	-- * Event
	getRandom, getRandomR ) where

import Control.Monad.State (StateT, gets, modify)
import Data.Type.Set (Set(Nil), Singleton, numbered, (:-))
import Data.OneOrMore (Selectable(..), singleton, extract)
import System.Random (Random, StdGen, random, randomR)

import Moffy.React
import Moffy.React.Common
import Moffy.Handle

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
