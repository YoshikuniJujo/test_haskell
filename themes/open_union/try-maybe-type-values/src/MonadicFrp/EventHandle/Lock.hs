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

import Control.Monad.State
import Data.Type.Set
import Data.UnionSet hiding (merge)
import Data.Bool
import MonadicFrp
import MonadicFrp.Handle
import MonadicFrp.EventHandle.ThreadId

newtype LockId = LockId Int deriving (Show, Eq)
type RetryTime = Int

class LockState s where
	getNextLockId :: s -> Int
	putNextLockId :: s -> Int -> s
	isLocked :: s -> LockId -> Bool
	lockIt :: s -> LockId -> s
	unlockIt :: s -> LockId -> s

newtype NewLockId = NewLockIdReq ThreadId deriving (Show, Eq)
numbered 9 [t| NewLockId |]
instance Mrgable NewLockId where nl1 `mrg` _nl2 = nl1
instance Request NewLockId where
	data Occurred NewLockId = OccNewLockId LockId ThreadId

newLockId :: React (GetThreadId :- NewLockId :- 'Nil) LockId
newLockId = adjust getThreadId >>= \ti ->
	maybe newLockId pure =<< adjust (await (NewLockIdReq ti)
		\(OccNewLockId l ti') -> bool Nothing (Just l) $ ti == ti')

handleNewLockId :: (LockState s, Monad m) => Handle' (StateT s m) (Singleton NewLockId)
handleNewLockId reqs = get >>= \s -> do
	let	i = getNextLockId s
		l = LockId i
	Just (singleton $ OccNewLockId l ti) <$ modify (`putNextLockId` (i + 1))
	where NewLockIdReq ti = extract reqs

data GetLock = GetLockReq LockId ThreadId RetryTime deriving (Show, Eq)
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

newtype Unlock = UnlockReq LockId deriving Show
numbered 9 [t| Unlock |]
instance Mrgable Unlock where ul1 `mrg` _ul2 = ul1
instance Request Unlock where data Occurred Unlock = OccUnlock

unlock :: LockId -> React (Singleton Unlock) ()
unlock l = await (UnlockReq l) (const ())

handleUnlock :: (LockState s, Monad m) => Handle' (StateT s m) (Singleton Unlock)
handleUnlock reqs = Just (singleton OccUnlock) <$ modify (`unlockIt` l)
	where UnlockReq l = extract reqs

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

type LockEv = NewLockId :- GetLock :- Unlock :- 'Nil

handleLock :: (LockState s, Monad m) => Handle' (StateT s m) LockEv
handleLock = handleNewLockId `merge` handleGetLock `merge` handleUnlock
