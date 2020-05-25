{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.NewRandom (
	RandomEv, RandomState(..), getRandom, getRandomR, handleRandomGen
	) where

import Control.Monad.State
import Data.Type.Set
import Data.UnionSet hiding (merge)
import System.Random

import MonadicFrp
import MonadicFrp.Handle
import Trials.Followbox.ThreadId
import Trials.Lock

class RandomState s where
	getRandomGen :: s -> StdGen
	putRandomGen :: s -> StdGen -> s

data StoreRandomGen = StoreRandomGenReq StdGen deriving Show
numbered 9 [t| StoreRandomGen |]
instance Mrgable StoreRandomGen where g1 `mrg` _g2 = g1
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen

storeRandomGen :: StdGen -> React (Singleton StoreRandomGen) ()
storeRandomGen g = await (StoreRandomGenReq g) (const ())

handleStoreRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton StoreRandomGen)
handleStoreRandomGen reqs = Just (singleton OccStoreRandomGen) <$ modify (`putRandomGen` g)
	where StoreRandomGenReq g = extract reqs

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered 9 [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGen deriving Show

loadRandomGen :: React (Singleton LoadRandomGen) StdGen
loadRandomGen = await LoadRandomGenReq \(OccLoadRandomGen g) -> g

handleLoadRandomGen :: (RandomState s, Monad m) =>
	Handle' (StateT s m) (Singleton LoadRandomGen)
handleLoadRandomGen _reqs = Just . singleton . OccLoadRandomGen <$> gets getRandomGen

getRandom :: Random a => LockId -> React (GetThreadId :- LockEv :+: RandomEv) a
getRandom l = atomicModifyRandomGen l random

getRandomR :: Random a =>
	LockId -> (a, a) -> React (GetThreadId :- LockEv :+: RandomEv) a
getRandomR l = atomicModifyRandomGen l . randomR
	
atomicModifyRandomGen :: LockId -> (StdGen -> (a, StdGen)) ->
	React (GetThreadId :- LockEv :+: StoreRandomGen :- LoadRandomGen :- 'Nil) a
atomicModifyRandomGen l f = withLock l $ modifyRandomGen f

modifyRandomGen :: (StdGen -> (a, StdGen)) -> React (StoreRandomGen :- LoadRandomGen :- 'Nil) a
modifyRandomGen f = do
	g <- adjust loadRandomGen
	let	(r, g') = f g
	r <$ adjust (storeRandomGen g')

handleRandomGen :: (RandomState s, Monad m) => Handle' (StateT s m) RandomEv
handleRandomGen = handleStoreRandomGen `merge` handleLoadRandomGen

type RandomEv = StoreRandomGen :- LoadRandomGen :- 'Nil
