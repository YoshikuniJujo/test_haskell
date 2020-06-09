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
import MonadicFrp.EventHandle.ThreadId
import MonadicFrp.EventHandle.Lock

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

type LockedInt' = GetThreadId :- LockEv :+: (StoreInt :- LoadInt :- 'Nil)

add1' :: LockId -> React LockedInt' Int
add1' l = withLock l add1

tryLock :: Sig LockedInt' (Int, Int, Int) ()
tryLock = do
	l <- waitFor $ adjust newLockId
	let	foo = repeat . adjust $ add1' l
	(,,) <$%> foo <*%> foo <*%> foo

handleStoreLoadIntWithLockNew :: (LockState s, IntState s, Monad m) => Handle (StateT s m) LockedInt'
handleStoreLoadIntWithLockNew = retry $
	handleGetThreadId `merge` handleLock `merge`
	handleStoreInt `merge` handleLoadInt

data LockIntState = LockIntState {
	nextLockId :: Int,
	lockState :: [LockId],
	intState :: Int } deriving Show

instance LockState LockIntState where
	getNextLockId = nextLockId
	putNextLockId s l = s { nextLockId = l }
	isLocked = flip elem . lockState
	lockIt s l = s { lockState = l : lockState s }
	unlockIt s l = s { lockState = delete l $ lockState s }

instance IntState LockIntState where
	getInt = intState
	putInt s n = s { intState = n }

initialLockIntState :: LockIntState
initialLockIntState =
	LockIntState { nextLockId = 0, lockState = [], intState = 0 }

handleStoreLoadIntWithLockNew' :: Handle (StateT LockIntState IO) LockedInt'
handleStoreLoadIntWithLockNew' reqs = do
	lift $ threadDelay 250000
	handleStoreLoadIntWithLockNew reqs
