{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CountWithLock where

import Control.Monad.State
import Data.Type.Set
import Data.OneOrMore hiding (merge)
import Data.Or
import Data.List

import Moffy.React
import Moffy.React.Common
import Moffy.Handle
import Moffy.EventHandle.ThreadId
import Moffy.EventHandle.Lock

data Count = CountReq deriving (Show, Eq, Ord)
numbered 9 [t| Count |]
instance Request Count where data Occurred Count = OccCount Int deriving Show

count :: React s (Singleton Count) Int
count = await CountReq \(OccCount n) -> n

class CountState s where getCount :: s -> Int; putCount :: s -> Int -> s
instance CountState Int where getCount = id; putCount = flip const

handleCount :: (Monad m, CountState s) => Handle' (StateT s m) (Singleton Count)
handleCount _rqs = Just . singleton . OccCount <$> do
	n <- gets getCount
	modify (`putCount` (n + 1))
	pure n

count2 :: React s (Singleton Count) (Or Int Int)
count2 = count `first` count

noLockCount :: (Or Int Int, Int)
noLockCount = interpretReact (retry handleCount) (count2 >> count2) `runState` 0

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

lockCount :: (Or Int Int, LockCountSt)
lockCount = (`runState` initLockCountSt) $ interpretReact (retry $ handleGetThreadId `merge` handleLock `merge` handleCount) do
	li <- adjust newLockId
	() <$ count2WithLock li -- :: React s (Count :- GetThreadId :- LockEv) (Or Int Int)
	count2WithLock li
