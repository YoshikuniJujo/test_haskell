{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Lock.Internal (
	-- * Type Synonym
	LockEv, GetThreadIdNewLockId, GetThreadIdGetLock, SingletonUnlock,
	-- * Event Type
	NewLockId(..), pattern OccNewLockId, GetLock(..), pattern OccGetLock,
	Unlock(..), pattern OccUnlock, LockId(..),
	-- * Event
	newLockId, withLock, withLockSig ) where

import Control.Moffy (Sig, React, Request(..), Adjustable, adjust, await, waitFor, adjustSig)
import Control.Moffy.Event.ThreadId (GetThreadId, getThreadId, ThreadId)
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-), (:+:))
import Data.OneOrMore (Selectable(..))
import Data.Bool (bool)

---------------------------------------------------------------------------

-- * LOCK ID
-- * EVENT
--	+ NEW LOCK ID
--	+ GET LOCK
--	+ UNLOCK
-- * WITH LOCK

---------------------------------------------------------------------------
-- LOCK ID
---------------------------------------------------------------------------

newtype LockId = LockId Int deriving (Show, Eq)

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- NEW LOCK ID

newtype NewLockId = NewLockIdReq ThreadId deriving (Show, Eq)
numbered [t| NewLockId |]
instance Selectable NewLockId where l `select` _r = l
instance Request NewLockId where
	data Occurred NewLockId = OccNewLockId LockId ThreadId

type GetThreadIdNewLockId = GetThreadId :- NewLockId :- 'Nil

newLockId :: React s GetThreadIdNewLockId LockId
newLockId = adjust getThreadId >>= \t -> newLockId `maybe` pure
	=<< (adjust . await (NewLockIdReq t))
		\(OccNewLockId i t') -> bool Nothing (Just i) $ t == t'

-- GET LOCK

data GetLock = GetLockReq LockId ThreadId RetryTime deriving (Show, Eq)
type RetryTime = Int
numbered [t| GetLock |]
instance Selectable GetLock where
	l@(GetLockReq _ _ rtl) `select` r@(GetLockReq _ _ rtr)
		| rtl >= rtr = l | otherwise = r
instance Request GetLock where
	data Occurred GetLock = OccGetLock LockId ThreadId

type GetThreadIdGetLock = GetThreadId :- GetLock :- 'Nil

getLock :: LockId -> RetryTime -> React s GetThreadIdGetLock ()
getLock i rt = adjust getThreadId >>= \t -> getLock i (rt + 1) `bool` pure ()
	=<< (adjust . await (GetLockReq i t rt))
		\(OccGetLock i' t') -> i == i' && t == t'

-- UNLOCK

newtype Unlock = UnlockReq LockId deriving Show
numbered [t| Unlock |]
instance Selectable Unlock where l `select` _r = l
instance Request Unlock where data Occurred Unlock = OccUnlock

type SingletonUnlock = Singleton Unlock

unlock :: LockId -> React s (Singleton Unlock) ()
unlock l = await (UnlockReq l) \OccUnlock -> ()

---------------------------------------------------------------------------
-- WITH LOCK
---------------------------------------------------------------------------

type LockEv = NewLockId :- GetLock :- Unlock :- 'Nil

withLock :: (
	(es :+: es') ~ es',
	(GetThreadIdGetLock :+: es') ~ es', (SingletonUnlock :+: es') ~ es',
	Adjustable es es',
	Adjustable GetThreadIdGetLock es', Adjustable SingletonUnlock es' ) =>
	LockId -> React s es a -> React s es' a
withLock l act = adjust (getLock l 0) >> adjust act <* adjust (unlock l)

withLockSig :: (
	(es :+: es') ~ es',
	(GetThreadIdGetLock :+: es') ~ es', (SingletonUnlock :+: es') ~ es',
	Adjustable es es',
	Adjustable GetThreadIdGetLock es', Adjustable SingletonUnlock es' ) =>
	LockId -> Sig s es a r -> Sig s es' a r
withLockSig l s = do
	waitFor . adjust $ getLock l 0
	adjustSig s <* waitFor (adjust $ unlock l)
