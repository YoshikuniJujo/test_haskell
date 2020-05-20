{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Random (
	-- * BASIC TYPES
	RandomEv, StdGenVersion, stdGenVersion0,
	-- * HANDLE
	RandomState(..), handleRandom,
	-- * REACT
	getRandom, getRandomR
	) where

import Control.Monad.State (StateT, gets, modify)
import Data.Type.Set (numbered, Set(Nil), Singleton, (:-))
import Data.UnionSet (Mrgable(..), singleton, extract)
import Data.Bool (bool)
import System.Random (Random, StdGen, random, randomR)

import MonadicFrp (Request(..), React, adjust, await)
import MonadicFrp.Run ()
import MonadicFrp.Handle (Handle, Handle', merge)
import MonadicFrp.ThreadId (ThreadId)
import Trials.Followbox.ThreadId (GetThreadId, getThreadId)

---------------------------------------------------------------------------

-- DEFINE EVENTS

data StdGenVersion = StdGenVersion Int deriving (Show, Eq, Ord)

stdGenVersion0 :: StdGenVersion
stdGenVersion0 = StdGenVersion 0

nextVersion :: StdGenVersion -> StdGenVersion
nextVersion (StdGenVersion v) = StdGenVersion $ v + 1

data StoreRandomGen = StoreRandomGen ThreadId StdGen deriving Show
numbered 8 [t| StoreRandomGen |]
instance Mrgable StoreRandomGen where g1 `mrg` _g2 = g1
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen ThreadId StdGenVersion
		deriving Show

storeRandomGen ::
	StdGen -> React (GetThreadId :- StoreRandomGen :- 'Nil) StdGenVersion
storeRandomGen g = adjust getThreadId >>= \ti ->
	maybe (storeRandomGen g) pure =<< adjust (await (StoreRandomGen ti g)
		\(OccStoreRandomGen ti' v) -> bool Nothing (Just v) $ ti == ti')

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered 8 [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGenVersion StdGen
		deriving Show

loadRandomGen :: React (Singleton LoadRandomGen) (StdGenVersion, StdGen)
loadRandomGen = await LoadRandomGenReq \(OccLoadRandomGen v g) -> (v, g)

data Rollback = RollbackReq StdGenVersion deriving (Show, Eq, Ord)
numbered 8 [t| Rollback |]
instance Request Rollback where
	data Occurred Rollback = OccRollback StdGenVersion deriving Show

rollback :: StdGenVersion -> React (Singleton Rollback) ()
rollback v = bool (rollback v) (pure ()) =<<
	await (RollbackReq v) \(OccRollback v') -> v == v'

type RandomEv = StoreRandomGen :- LoadRandomGen :- Rollback :- 'Nil

-- HANDLE EVENTS

class RandomState s where
	getVersionStdGen :: s -> (StdGenVersion, StdGen)
	putVersionStdGen :: s -> (StdGenVersion, StdGen) -> s
	rollbackStdGen :: s -> StdGenVersion -> Either String s

handleStoreRandomGen :: (Monad m, RandomState s) =>
	Handle (StateT s m) (Singleton StoreRandomGen)
handleStoreRandomGen reqs = gets (fst . getVersionStdGen) >>= \v ->
	singleton (OccStoreRandomGen ti v)
		<$ modify (`putVersionStdGen` (nextVersion v, g))
	where StoreRandomGen ti g = extract reqs

handleLoadRandomGen :: (Monad m, RandomState s) =>
	Handle (StateT s m) (Singleton LoadRandomGen)
handleLoadRandomGen _reqs =
	singleton . uncurry OccLoadRandomGen <$> gets getVersionStdGen

handleRollback ::
	(Monad m, RandomState s) => Handle (StateT s m) (Singleton Rollback)
handleRollback reqs = singleton (OccRollback v)
	<$ modify (either error id . (`rollbackStdGen` v))
	where RollbackReq v = extract reqs

handleRandom :: (Monad m, RandomState s) => Handle' (StateT s m) RandomEv
handleRandom =
	(Just <$>) . handleStoreRandomGen `merge`
	(Just <$>) . handleLoadRandomGen `merge`
	(Just <$>) . handleRollback

-- GET RANDOM

getRandom :: Random a => React (GetThreadId :- RandomEv) a
getRandom = atomicModifyRandomGen random

getRandomR :: Random a => (a, a) -> React (GetThreadId :- RandomEv) a
getRandomR = atomicModifyRandomGen . randomR

atomicModifyRandomGen :: (StdGen -> (a, StdGen)) -> React
	(GetThreadId :- StoreRandomGen :- LoadRandomGen :- Rollback :- 'Nil) a
atomicModifyRandomGen f = adjust loadRandomGen >>= \(v, g) -> do
	let	(r, g') = f g
	adjust (storeRandomGen g') >>= \v' -> flip (uncurry bool) (v == v')
		(adjust (rollback v') >> atomicModifyRandomGen f, pure r)
