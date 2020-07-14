{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Lock.Internal (
	-- * Type
	LockEv, LockState(..), LockId(..),
	NewLockId(..), pattern OccNewLockId, GetLock(..), pattern OccGetLock,
	Unlock(..), pattern OccUnlock,
	-- * Event
	GetThreadIdNewLockId, newLockId,
	GetThreadIdGetLock, SingletonUnlock, withLock ) where

import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.OneOrMore (Selectable(..))
import Data.Bool (bool)

import Control.Moffy (React, Request(..), Adjustable, adjust, await)
import Control.Moffy.Event.ThreadId (GetThreadId, ThreadId, getThreadId)

---------------------------------------------------------------------------

-- * LOCK STATE AND LOCK ID
-- * EVENT
--	+ NEW LOCK ID
--	+ GET LOCK
--	+ UNLOCK
-- * WITHLOCK

---------------------------------------------------------------------------
-- LOCK STATE AND LOCK ID
---------------------------------------------------------------------------

class LockState s where
	getNextLockId :: s -> Int; putNextLockId :: s -> Int -> s
	isLocked :: s -> LockId -> Bool
	lockIt :: s -> LockId -> s; unlockIt :: s -> LockId -> s

newtype LockId = LockId Int deriving (Show, Eq)

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- NEW LOCK ID

newtype NewLockId = NewLockIdReq ThreadId deriving (Show, Eq)
numbered 9 [t| NewLockId |]
instance Selectable NewLockId where l `select` _r = l
instance Request NewLockId where
	data Occurred NewLockId = OccNewLockId LockId ThreadId

type GetThreadIdNewLockId = GetThreadId :- NewLockId :- 'Nil

newLockId :: React s GetThreadIdNewLockId LockId
newLockId = adjust getThreadId >>= \t -> maybe newLockId pure =<< adjust (
	await (NewLockIdReq t)
		\(OccNewLockId i t') -> bool Nothing (Just i) $ t == t' )

-- GET LOCK

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
	(es :+: es') ~ es',
	(GetThreadIdGetLock :+: es') ~ es', (SingletonUnlock :+: es') ~ es',
	Adjustable es es',
	Adjustable GetThreadIdGetLock es', Adjustable SingletonUnlock es') =>
	LockId -> React s es a -> React s es' a
withLock l act = adjust (getLock l 0) >> adjust act <* adjust (unlock l)
