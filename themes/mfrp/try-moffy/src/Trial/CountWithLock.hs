{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CountWithLock where

import Control.Moffy
import Data.Type.Set
import Data.Or
import Data.List

import Control.Moffy.Handle.ThreadId
import Control.Moffy.Handle.Lock
import Control.Moffy.Event.Lock

data Count = CountReq deriving (Show, Eq, Ord)
numbered [t| Count |]
instance Request Count where data Occurred Count = OccCount Int deriving Show

count :: React s (Singleton Count) Int
count = await CountReq \(OccCount n) -> n

class CountState s where getCount :: s -> Int; putCount :: s -> Int -> s
instance CountState Int where getCount = id; putCount = flip const

count2 :: React s (Singleton Count) (Or Int Int)
count2 = count `first` count

count2WithLock :: LockId -> React s (Count :- GetThreadId :- LockEv) (Or Int Int)
count2WithLock li =
	(withLock li count :: React s (Count :- GetThreadId :- LockEv) Int)
	`first`
	(withLock li count :: React s (Count :- GetThreadId :- LockEv) Int)

data LockCountSt = LockCountSt {
	counter :: Int, nextLockId :: Int, lockState :: [LockId]
	} deriving Show

initLockCountSt :: LockCountSt
initLockCountSt = LockCountSt 0 0 []

instance LockState LockCountSt where
	getNextLockId = nextLockId
	putNextLockId s li = s { nextLockId = li }
	isLocked LockCountSt { lockState = ls } li = li `elem` ls
	lockIt s li = s { lockState = li : lockState s }
	unlockIt s li = s { lockState = delete li (lockState s) }

instance CountState LockCountSt where
	getCount = counter
	putCount s n = s { counter = n }
