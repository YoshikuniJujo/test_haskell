{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.EventHandle.Lock (
	-- * Type
	LockEv, NewLockId, GetLock, Unlock, LockId, LockState(..),
	-- * Handle
	handleLock,
	-- * Event
	GetThreadIdGetLock, SingletonUnlock, newLockId, withLock ) where

import Control.Monad.State (StateT, get, modify)
import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.UnionSet (Mrgable(..), Expandable, singleton, extract)
import Data.Bool (bool)

import MonadicFrp (Request(..), React, Firstable, await, adjust)
import MonadicFrp.Handle (Handle', merge)
import MonadicFrp.EventHandle.ThreadId (GetThreadId, ThreadId, getThreadId)

---------------------------------------------------------------------------

-- * LOCKID AND LOCKSTATE
-- * EVENT
--	+ NEWLOCKID
--	+ GETLOCK
--	+ UNLOCK
-- * LOCKEV
-- * WITHLOCK

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
instance Mrgable NewLockId where i1 `mrg` _i2 = i1
instance Request NewLockId where
	data Occurred NewLockId = OccNewLockId LockId ThreadId

newLockId :: React (GetThreadId :- NewLockId :- 'Nil) LockId
newLockId = adjust getThreadId >>= \ti ->
	maybe newLockId pure =<< adjust (await (NewLockIdReq ti)
		\(OccNewLockId i ti') -> bool Nothing (Just i) $ ti == ti')

handleNewLockId ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton NewLockId)
handleNewLockId rqs = get >>= \s -> let i = getNextLockId s in
	Just (singleton $ OccNewLockId (LockId i) ti)
		<$ modify (`putNextLockId` (i + 1))
	where NewLockIdReq ti = extract rqs

-- GETLOCK

data GetLock = GetLockReq LockId ThreadId RetryTime deriving (Show, Eq)
type RetryTime = Int
numbered 9 [t| GetLock |]
instance Mrgable GetLock where
	gl1@(GetLockReq _ _ rt1) `mrg` gl2@(GetLockReq _ _ rt2)
		| rt1 >= rt2 = gl1
		| otherwise = gl2
instance Request GetLock where
	data Occurred GetLock = OccGetLock LockId ThreadId

getLock :: LockId -> RetryTime -> React (GetThreadId :- GetLock :- 'Nil) ()
getLock l rt = adjust getThreadId >>= \ti ->
	maybe (pure ()) (getLock l) =<< adjust (await (GetLockReq l ti rt)
		\(OccGetLock l' ti') -> if l == l' && ti == ti' then Nothing else Just $ rt + 1)

handleGetLock :: (LockState s, Monad m) => Handle' (StateT s m) (Singleton GetLock)
handleGetLock reqs = get >>= \s ->
	bool (Just (singleton $ OccGetLock l ti) <$ modify (`lockIt` l)) (pure Nothing) $ s `isLocked` l
	where GetLockReq l ti _ = extract reqs

-- UNLOCK

newtype Unlock = UnlockReq LockId deriving Show
numbered 9 [t| Unlock |]
instance Mrgable Unlock where ul1 `mrg` _ul2 = ul1
instance Request Unlock where data Occurred Unlock = OccUnlock

unlock :: LockId -> React (Singleton Unlock) ()
unlock l = await (UnlockReq l) (const ())

handleUnlock :: (LockState s, Monad m) => Handle' (StateT s m) (Singleton Unlock)
handleUnlock reqs = Just (singleton OccUnlock) <$ modify (`unlockIt` l)
	where UnlockReq l = extract reqs

---------------------------------------------------------------------------
-- LOCKEV
---------------------------------------------------------------------------

type LockEv = NewLockId :- GetLock :- Unlock :- 'Nil

handleLock :: (LockState s, Monad m) => Handle' (StateT s m) LockEv
handleLock = handleNewLockId `merge` handleGetLock `merge` handleUnlock

---------------------------------------------------------------------------
-- WITHLOCK
---------------------------------------------------------------------------

type GetThreadIdGetLock = GetThreadId :- GetLock :- 'Nil
type SingletonUnlock = Singleton Unlock

withLock :: (
	(es :+: es') ~ es',
	(GetThreadIdGetLock :+: es') ~ es', (SingletonUnlock :+: es') ~ es',
	Expandable es es',
	Expandable GetThreadIdGetLock es', Expandable SingletonUnlock es',
	Firstable es es',
	Firstable GetThreadIdGetLock es', Firstable SingletonUnlock es' ) =>
	LockId -> React es a -> React es' a
withLock l act = adjust (getLock l 0) >> adjust act <* adjust (unlock l)
