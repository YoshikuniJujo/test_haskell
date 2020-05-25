{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Lock.Try where

import Prelude hiding (repeat)

import Control.Monad.State
import Control.Concurrent
import Data.Type.Set
import Data.Type.Flip
import Data.UnionSet hiding (merge)
import Data.List (delete)

import MonadicFrp
import MonadicFrp.Handle
import Trials.Lock
import Trials.Followbox.ThreadId

class IntState s where
	getInt :: s -> Int
	putInt :: s -> Int -> s

instance IntState Int where
	getInt = id
	putInt = flip const

data StoreInt = StoreIntReq Int deriving (Show, Eq, Ord)
numbered 9 [t| StoreInt |]
instance Request StoreInt where
	data Occurred StoreInt = OccStoreInt

storeInt :: Int -> React (Singleton StoreInt) ()
storeInt n = await (StoreIntReq n) (const ())

data LoadInt = LoadIntReq deriving (Show, Eq, Ord)
numbered 9 [t| LoadInt |]
instance Request LoadInt where
	data Occurred LoadInt = OccLoadInt Int deriving Show

loadInt :: React (Singleton LoadInt) Int
loadInt = await LoadIntReq \(OccLoadInt n) -> n

handleStoreInt :: (IntState s, Monad m) => Handle' (StateT s m) (Singleton StoreInt)
handleStoreInt reqs = Just (singleton OccStoreInt) <$ modify (`putInt` n)
	where StoreIntReq n = extract reqs

handleLoadInt :: (IntState s, Monad m) => Handle' (StateT s m) (Singleton LoadInt)
handleLoadInt _reqs = Just . singleton . OccLoadInt <$> gets getInt

handleStoreLoadInt :: Monad m => Handle (StateT Int m) (StoreInt :- LoadInt :- 'Nil)
handleStoreLoadInt = retry $ handleStoreInt `merge` handleLoadInt

handleStoreLoadInt' :: Handle (StateT Int IO) (StoreInt :- LoadInt :- 'Nil)
handleStoreLoadInt' reqs = do
	lift $ threadDelay 500000
	handleStoreLoadInt reqs

add1 :: React (StoreInt :- LoadInt :- 'Nil) Int
add1 = do
	n <- adjust loadInt
	let	n' = n + 1
	adjust $ storeInt n'
	pure n'

repeatAdd1 :: Sig (StoreInt :- LoadInt :- 'Nil) Int ()
repeatAdd1 = repeat add1

add1add1add1 :: Sig (StoreInt :- LoadInt :- 'Nil) (Int, Int, Int) ()
add1add1add1 = (,,) <$%> repeatAdd1 <*%> repeatAdd1 <*%> repeatAdd1

type LockedInt = LockEv :+: (StoreInt :- LoadInt :- 'Nil) -- GetThreadId :- GetLock :- Unlock :- StoreInt :- LoadInt :- 'Nil
type LockedInt' = LockEv' :+: (StoreInt :- LoadInt :- 'Nil)
--	GetThreadId :- NewLockId :- GetLock :- Unlock :- StoreInt :- LoadInt :-
--	'Nil

add1' :: LockId -> React LockedInt Int
add1' l = withLock l add1

repeatAdd1' :: LockId -> Sig LockedInt Int ()
repeatAdd1' l = repeat $ add1' l

add1add1 :: LockId -> Sig LockedInt (Int, Int) ()
add1add1 l = (,) <$%> repeatAdd1' l <*%> repeatAdd1' l

add1add1add1' :: LockId -> Sig LockedInt (Int, Int, Int) ()
add1add1add1' l = (,,) <$%> repeatAdd1' l <*%> repeatAdd1' l <*%> repeatAdd1' l

tryLock :: Sig LockedInt' (Int, Int, Int) ()
tryLock = do
	l <- waitFor $ adjust newLockId
	let	foo = repeat . adjust $ add1' l
	(,,) <$%> foo <*%> foo <*%> foo

handleStoreLoadIntWithLock :: (LockState s, IntState s, Monad m) => Handle (StateT s m) LockedInt
handleStoreLoadIntWithLock = retry $
	handleGetThreadId `merge`
	handleGetLock `merge`
	handleUnlock `merge`
	handleStoreInt `merge`
	handleLoadInt

handleStoreLoadIntWithLockNew :: (LockState s, IntState s, Monad m) => Handle (StateT s m) LockedInt'
handleStoreLoadIntWithLockNew = retry $
	handleNewLockId `merge`
	handleGetThreadId `merge`
	handleGetLock `merge`
	handleUnlock `merge`
	handleStoreInt `merge`
	handleLoadInt

data LockIntState = LockIntState {
	nextLockId :: Int,
	lockState :: [LockId],
	intState :: Int } deriving Show

instance LockState LockIntState where
	isLocked = flip elem . lockState
	lockIt s l = s { lockState = l : lockState s }
	unlockIt s l = s { lockState = delete l $ lockState s }
	getLockId = nextLockId
	putLockId s l = s { nextLockId = l }

instance IntState LockIntState where
	getInt = intState
	putInt s n = s { intState = n }

initialLockIntState :: LockIntState
initialLockIntState =
	LockIntState { nextLockId = 0, lockState = [], intState = 0 }

handleStoreLoadIntWithLock' :: Handle (StateT LockIntState IO) LockedInt
handleStoreLoadIntWithLock' reqs = do
	lift $ threadDelay 250000
	handleStoreLoadIntWithLock reqs

handleStoreLoadIntWithLockNew' :: Handle (StateT LockIntState IO) LockedInt'
handleStoreLoadIntWithLockNew' reqs = do
	lift $ threadDelay 250000
	handleStoreLoadIntWithLockNew reqs
