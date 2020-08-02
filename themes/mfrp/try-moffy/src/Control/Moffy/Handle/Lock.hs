{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Lock (
	-- * Type
	LockEv, LockState(..), LockId,
	-- * Handle
	handleLock ) where

import Control.Moffy.Event.Lock.Internal (
	LockEv, LockId(..), NewLockId(..), pattern OccNewLockId,
	GetLock(..), pattern OccGetLock, Unlock(..), pattern OccUnlock )
import Control.Moffy.Handle (Handle', merge)
import Control.Monad.State (MonadState(..), gets, modify)
import Data.Type.Set (Singleton)
import Data.OneOrMore (pattern Singleton)
import Data.Bool (bool)

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

handleLock :: (LockState (StateType m), Monad m, MonadState m) => Handle' m LockEv
handleLock = handleNewLockId `merge` handleGetLock `merge` handleUnlock

handleNewLockId ::
	(LockState (StateType m), Monad m, MonadState m) => Handle' m (Singleton NewLockId)
handleNewLockId (Singleton (NewLockIdReq t)) = gets getNextLockId >>= \i ->
	Just (Singleton $ OccNewLockId (LockId i) t)
		<$ modify (`putNextLockId` (i + 1))

handleGetLock ::
	(LockState (StateType m), Monad m, MonadState m) => Handle' m (Singleton GetLock)
handleGetLock (Singleton (GetLockReq l t _)) = gets (`isLocked` l) >>= bool
	(Just (Singleton $ OccGetLock l t) <$ modify (`lockIt` l))
	(pure Nothing)

handleUnlock ::
	(LockState (StateType m), Monad m, MonadState m) => Handle' m (Singleton Unlock)
handleUnlock (Singleton (UnlockReq l)) =
	Just (Singleton OccUnlock) <$ modify (`unlockIt` l)
