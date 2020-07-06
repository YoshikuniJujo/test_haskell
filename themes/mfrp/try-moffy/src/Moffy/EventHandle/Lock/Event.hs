{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.EventHandle.Lock.Event (
	-- * Type
	LockEv, NewLockId(..), GetLock(..), Unlock(..), LockId(..), LockState(..),
	Occurred(OccNewLockId, OccGetLock, OccUnlock),
	-- * Event
	GetThreadIdGetLock, SingletonUnlock, newLockId, withLock ) where

import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.OneOrMore hiding (merge)
import Data.Bool (bool)

import Control.Moffy
import Control.Moffy.Event.ThreadId

---------------------------------------------------------------------------

-- * LOCKID AND LOCKSTATE
-- * EVENT
--	+ NEWLOCKID
--	+ GETLOCK
--	+ UNLOCK
-- * HANDLE AND WITHLOCK

---------------------------------------------------------------------------
-- LOCKID AND LOCKSTATE
---------------------------------------------------------------------------

newtype LockId = LockId Int deriving (Show, Eq)

class LockState s where
	getNextLockId :: s -> Int; putNextLockId :: s -> Int -> s
	isLocked :: s -> LockId -> Bool
	lockIt :: s -> LockId -> s; unlockIt :: s -> LockId -> s

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- NEWLOCKID

newtype NewLockId = NewLockIdReq ThreadId deriving (Show, Eq)
numbered 9 [t| NewLockId |]
instance Selectable NewLockId where i1 `select` _i2 = i1
instance Request NewLockId where
	data Occurred NewLockId = OccNewLockId LockId ThreadId

newLockId :: React s (GetThreadId :- NewLockId :- 'Nil) LockId
newLockId = adjust getThreadId >>= \ti ->
	maybe newLockId pure =<< adjust (await (NewLockIdReq ti)
		\(OccNewLockId i ti') -> bool Nothing (Just i) $ ti == ti')

-- GETLOCK

data GetLock = GetLockReq LockId ThreadId RetryTime deriving (Show, Eq)
type RetryTime = Int
numbered 9 [t| GetLock |]
instance Selectable GetLock where
	g1@(GetLockReq _ _ rt1) `select` g2@(GetLockReq _ _ rt2)
		| rt1 >= rt2 = g1 | otherwise = g2
instance Request GetLock where
	data Occurred GetLock = OccGetLock LockId ThreadId

type GetThreadIdGetLock = GetThreadId :- GetLock :- 'Nil

getLock :: LockId -> RetryTime -> React s GetThreadIdGetLock ()
getLock l rt = adjust getThreadId >>= \ti ->
	maybe (pure ()) (getLock l) =<< adjust (await (GetLockReq l ti rt)
		\(OccGetLock l' ti') ->
			bool (Just $ rt + 1) Nothing $ l == l' && ti == ti')

-- UNLOCK

newtype Unlock = UnlockReq LockId deriving Show
numbered 9 [t| Unlock |]
instance Selectable Unlock where u1 `select` _u2 = u1
instance Request Unlock where data Occurred Unlock = OccUnlock

type SingletonUnlock = Singleton Unlock

{-# ANN unlock "HLint: ignore Use const" #-}

unlock :: LockId -> React s (Singleton Unlock) ()
unlock l = await (UnlockReq l) \_ -> ()

---------------------------------------------------------------------------
-- WITHLOCK
---------------------------------------------------------------------------

type LockEv = NewLockId :- GetLock :- Unlock :- 'Nil

withLock :: (
	Adjustable GetThreadIdGetLock es',
	Adjustable es es',
	Adjustable SingletonUnlock es',
--	CollapsableOccurred es' GetThreadIdGetLock,
--	CollapsableOccurred es' es,
--	CollapsableOccurred es' SingletonUnlock,
	(es :+: es') ~ es',
	(GetThreadIdGetLock :+: es') ~ es', (SingletonUnlock :+: es') ~ es'
--	Expandable es es',
--	Expandable GetThreadIdGetLock es', Expandable SingletonUnlock es'
	) => LockId -> React s es a -> React s es' a
withLock l act = adjust (getLock l 0) >> adjust act <* adjust (unlock l)
