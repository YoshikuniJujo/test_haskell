{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Lock.Internal (
	-- * Type
	-- ** Type Synonym
	LockEv, GetThreadIdNewLockId, GetThreadIdGetLock, SingletonUnlock,
	RetryTime,
	-- ** Event Type
	NewLockId(..), pattern OccNewLockId, GetLock(..), pattern OccGetLock,
	Unlock(..), pattern OccUnlock,
	LockId(..),
	-- * Event
	newLockId, withLock ) where

import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.OneOrMore (Selectable(..))
import Data.Bool (bool)

import Control.Moffy (React, Request(..), Adjustable, adjust, await)
import Control.Moffy.Event.ThreadId (GetThreadId, ThreadId, getThreadId)

---------------------------------------------------------------------------

-- * LOCK ID
-- * EVENT
--	+ NEW LOCK ID
--	+ GET LOCK
--	+ UNLOCK
-- * WITHLOCK

---------------------------------------------------------------------------
-- LOCK ID
---------------------------------------------------------------------------

newtype LockId = LockId Int deriving (Show, Eq)

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- NEW LOCK ID

newtype NewLockId = NewLockIdReq ThreadId deriving (Show, Eq)
numbered 64 [t| NewLockId |]
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
numbered 64 [t| GetLock |]
instance Selectable GetLock where
	l@(GetLockReq _ _ rl) `select` r@(GetLockReq _ _ rr)
		| rl >= rr = l | otherwise = r
instance Request GetLock where
	data Occurred GetLock = OccGetLock LockId ThreadId

type GetThreadIdGetLock = GetThreadId :- GetLock :- 'Nil

getLock :: LockId -> RetryTime -> React s GetThreadIdGetLock ()
getLock l rt = adjust getThreadId >>= \t ->
	maybe (pure ()) (getLock l) =<< adjust (await (GetLockReq l t rt)
		\(OccGetLock l' t') ->
			bool (Just $ rt + 1) Nothing $ l == l' && t == t')

-- UNLOCK

newtype Unlock = UnlockReq LockId deriving Show
numbered 64 [t| Unlock |]
instance Selectable Unlock where l `select` _r = l
instance Request Unlock where data Occurred Unlock = OccUnlock

type SingletonUnlock = Singleton Unlock

unlock :: LockId -> React s (Singleton Unlock) ()
unlock l = await (UnlockReq l) \OccUnlock -> ()

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
