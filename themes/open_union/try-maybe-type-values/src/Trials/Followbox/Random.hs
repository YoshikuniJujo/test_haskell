{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox.Random (
	RandomState(..), RandomM, RandomEv, handle, getRandom, getRandomR,
	StdGenVersion, version0, StdRandomState, initialStdRandomState ) where

import Control.Monad.State
import Data.Type.Set
import Data.UnionSet hiding (merge)
import Data.Bool
import System.Random

import MonadicFrp
import MonadicFrp.Run
import MonadicFrp.ThreadId
import MonadicFrp.Handle

import Trials.TryThreadId hiding (sample1)

data StdGenVersion = StdGenVersion Int deriving (Show, Eq, Ord)

version0 :: StdGenVersion
version0 = StdGenVersion 0

nextVersion :: StdGenVersion -> StdGenVersion
nextVersion (StdGenVersion v) = StdGenVersion $ v + 1

data StoreRandomGen = StoreRandomGen ThreadId StdGen deriving Show
numbered 4 [t| StoreRandomGen |]
instance Mrgable StoreRandomGen where sg1 `mrg` _srgg2 = sg1
instance Request StoreRandomGen where
	data Occurred StoreRandomGen = OccStoreRandomGen ThreadId StdGenVersion deriving Show

storeRandomGen :: StdGen -> React (GetThreadId :- StoreRandomGen :- 'Nil) StdGenVersion
storeRandomGen g = do
	ti <- adjust getThreadId
	r <- adjust $ await (StoreRandomGen ti g)
		\(OccStoreRandomGen ti' v) -> bool Nothing (Just v) $ ti == ti'
	maybe (storeRandomGen g) pure r

data LoadRandomGen = LoadRandomGenReq deriving (Show, Eq, Ord)
numbered 4 [t| LoadRandomGen |]
instance Request LoadRandomGen where
	data Occurred LoadRandomGen = OccLoadRandomGen StdGenVersion StdGen deriving Show

loadRandomGen :: React (Singleton LoadRandomGen) (StdGenVersion, StdGen)
loadRandomGen = await LoadRandomGenReq \(OccLoadRandomGen v g) -> (v, g)

data Rollback = RollbackReq StdGenVersion deriving (Show, Eq, Ord)
numbered 4 [t| Rollback |]
instance Request Rollback where
	data Occurred Rollback = OccRollback deriving Show

rollback :: StdGenVersion -> React (Singleton Rollback) ()
rollback v = await (RollbackReq v) \OccRollback -> ()

atomicModifyRandomGen :: (StdGen -> (a, StdGen)) ->
	React (GetThreadId :- StoreRandomGen :- LoadRandomGen :- Rollback :- 'Nil) a
atomicModifyRandomGen f = do
	(v, g) <- adjust $ loadRandomGen
	let	(r, g') = f g
	v' <- adjust $ storeRandomGen g'
	bool (adjust (rollback v') >> atomicModifyRandomGen f) (pure r) $ v == v'

type RandomEv = GetThreadId :- StoreRandomGen :- LoadRandomGen :- Rollback :- 'Nil

handleGetThreadId :: Applicative m => Handle m (Singleton GetThreadId)
handleGetThreadId _reqs = pure $ singleton OccGetThreadId

handleStoreRandomGen :: (Monad m, RandomState s) => Handle (StateT s  m) (Singleton StoreRandomGen)
handleStoreRandomGen reqs = do
	v <- gets $ fst . getVersionStdGen
	modify $ (`putVersionStdGen` (nextVersion v, g)) -- ((nextVersion v, g) :)
	pure . singleton $ OccStoreRandomGen ti v
	where StoreRandomGen ti g = extract reqs

handleLoadRandomGen :: (Monad m, RandomState s) => Handle (StateT s  m) (Singleton LoadRandomGen)
handleLoadRandomGen _reqs = do
	vg <- gets getVersionStdGen
	pure . singleton $ uncurry OccLoadRandomGen vg

handleRollback :: (Monad m, RandomState s) => Handle (StateT s m) (Singleton Rollback)
handleRollback reqs = do
	modify $ either error id . (`rollbackStdGen` v)
	pure $ singleton OccRollback
	where RollbackReq v = extract reqs

handle :: (Monad m, RandomState s) => Handle (StateT s  m) RandomEv
-- handle :: Monad m => Handle (RandomM m) RandomEv
handle = retry $
	(Just <$>) . handleGetThreadId `merge`
	(Just <$>) . handleStoreRandomGen `merge`
	(Just <$>) . handleLoadRandomGen `merge`
	(Just <$>) . handleRollback

getRandom :: Random a => React RandomEv a
getRandom = atomicModifyRandomGen random

getRandomR :: Random a => (a, a) -> React RandomEv a
getRandomR = atomicModifyRandomGen . randomR

data StdRandomState = StdRandomState { getStdRandomState :: [(StdGenVersion, StdGen)] }

initialStdRandomState :: StdRandomState
initialStdRandomState = StdRandomState [(version0, mkStdGen 8)]

type RandomM = StateT StdRandomState -- [(StdGenVersion, StdGen)]

class RandomState s where
	getVersionStdGen :: s -> (StdGenVersion, StdGen)
	putVersionStdGen :: s -> (StdGenVersion, StdGen) -> s
	rollbackStdGen :: s -> StdGenVersion -> Either String s

instance RandomState StdRandomState where
	getVersionStdGen = head . getStdRandomState
	putVersionStdGen s = StdRandomState . (: getStdRandomState s)
	rollbackStdGen (StdRandomState ((v0, _) : vgs)) v
		| v == v0 = Right $ StdRandomState vgs
		| otherwise = Left "can't rollback"
	rollbackStdGen (StdRandomState []) _ = Left "can't rollback"
