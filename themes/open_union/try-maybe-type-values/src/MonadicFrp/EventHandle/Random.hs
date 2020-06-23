{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.EventHandle.Random (
	-- * Type
	RandomEv, RandomState(..),
	-- * Handle
	handleRandom,
	-- * Event
	getRandom, getRandomR ) where

import Control.Monad.State (StateT, gets, modify)
import Data.Type.Set (Set(Nil), Singleton, numbered, (:-))
import Data.UnionSet (Mrgable(..), singleton, extract)
import System.Random (Random, StdGen, random, randomR)

import MonadicFrp (Request, Occurred, React, await, adjust)
import MonadicFrp.Handle (Handle', merge)

---------------------------------------------------------------------------

-- STORE RANDOM GEN

newtype StoreRandomGen = StoreRandomGenReq StdGen deriving Show
numbered 9 [t| StoreRandomGen |]
instance Mrgable StoreRandomGen where gl `mrg` _gr = gl
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen

{-# ANN storeRandomGen "HLint: ignore Use const" #-}

storeRandomGen :: StdGen -> React (Singleton StoreRandomGen) ()
storeRandomGen g = await (StoreRandomGenReq g) \_ -> ()

-- LOAD RANDOM GEN

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered 9 [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGen deriving Show

loadRandomGen :: React (Singleton LoadRandomGen) StdGen
loadRandomGen = await LoadRandomGenReq \(OccLoadRandomGen g) -> g

-- RANDOM EVENT

type RandomEv = StoreRandomGen :- LoadRandomGen :- 'Nil

-- GET RANDOM

getRandom :: Random a => React RandomEv a
getRandom = modifyRandomGen random

getRandomR :: Random a => (a, a) -> React RandomEv a
getRandomR = modifyRandomGen . randomR

modifyRandomGen :: (StdGen -> (a, StdGen)) -> React RandomEv a
modifyRandomGen f = do
	(r, g') <- f <$> adjust loadRandomGen
	r <$ adjust (storeRandomGen g')

-- HANDLE

class RandomState s where
	getRandomGen :: s -> StdGen
	putRandomGen :: s -> StdGen -> s

handleRandom :: (RandomState s, Monad m) => Handle' (StateT s m) RandomEv
handleRandom = handleStoreRandomGen `merge` handleLoadRandomGen

handleStoreRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton StoreRandomGen)
handleStoreRandomGen reqs =
	Just (singleton OccStoreRandomGen) <$ modify (`putRandomGen` g)
	where StoreRandomGenReq g = extract reqs

handleLoadRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton LoadRandomGen)
handleLoadRandomGen _reqs =
	gets $ Just . singleton . OccLoadRandomGen . getRandomGen
