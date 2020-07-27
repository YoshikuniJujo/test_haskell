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

import Control.Moffy.Handle (Handle', merge)
import Control.Moffy.Event.Lock.Internal (
	LockEv, LockId(..), NewLockId(..), pattern OccNewLockId,
	GetLock(..), pattern OccGetLock, Unlock(..), pattern OccUnlock )
import Control.Monad.State (StateT, gets, modify)
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

handleLock :: (LockState s, Monad m) => Handle' (StateT s m) LockEv
handleLock = handleNewLockId `merge` handleGetLock `merge` handleUnlock

handleNewLockId ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton NewLockId)
handleNewLockId (Singleton (NewLockIdReq ti)) = gets getNextLockId >>= \i ->
	Just (Singleton $ OccNewLockId (LockId i) ti)
		<$ modify (`putNextLockId` (i + 1))

handleGetLock ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton GetLock)
handleGetLock (Singleton (GetLockReq l t _)) = gets (`isLocked` l) >>= bool
	(Just (Singleton $ OccGetLock l t) <$ modify (`lockIt` l))
	(pure Nothing)

handleUnlock ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton Unlock)
handleUnlock (Singleton (UnlockReq l)) =
	Just (Singleton OccUnlock) <$ modify (`unlockIt` l)
