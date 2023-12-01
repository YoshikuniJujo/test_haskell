{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Lock (
	-- * Type
	LockEv, LockState(..), LockId,
	-- * Handle
	handleLock ) where

import Control.Moffy.Event.Lock.Internal (
	LockEv, LockId(..), NewLockId(..), pattern OccNewLockId,
	GetLock(..), pattern OccGetLock, Unlock(..), pattern OccUnlock )
import Control.Moffy.Handle (HandleSt', mergeSt)
import Data.Type.Set (Singleton)
import Data.OneOrMore as Oom (pattern Singleton)
import Data.Bool (bool)

import Data.OneOrMoreApp as Ooma (pattern Singleton)

---------------------------------------------------------------------------

-- * LOCK STATE
-- * HANDLE

---------------------------------------------------------------------------
-- LOCK STATE
---------------------------------------------------------------------------

class LockState s where
	getNextLockId :: s -> Int; putNextLockId :: s -> Int -> s
	isLocked :: s -> LockId -> Bool
	lockIt :: s -> LockId -> s; unlockIt :: s -> LockId -> s

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

handleLock :: (LockState s, Monad m) => HandleSt' s m LockEv
handleLock = handleNewLockId `mergeSt` handleGetLock `mergeSt` handleUnlock

handleNewLockId ::
	(LockState s, Applicative m) => HandleSt' s m (Singleton NewLockId)
handleNewLockId (Oom.Singleton (NewLockIdReq t)) s = pure (
	Just . Ooma.Singleton $ OccNewLockId (LockId i) t,
	s `putNextLockId` (i + 1) )
	where i = getNextLockId s

handleGetLock ::
	(LockState s, Applicative m) => HandleSt' s m (Singleton GetLock)
handleGetLock (Oom.Singleton (GetLockReq i t _)) s = pure $ bool
	(Just . Ooma.Singleton $ OccGetLock i t, s `lockIt` i) (Nothing, s)
	(s `isLocked` i)

handleUnlock :: (LockState s, Applicative m) => HandleSt' s m (Singleton Unlock)
handleUnlock (Oom.Singleton (UnlockReq i)) s =
	pure (Just $ Ooma.Singleton OccUnlock, s `unlockIt` i)
