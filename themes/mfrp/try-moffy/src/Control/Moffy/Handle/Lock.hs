{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.Lock (
	-- * Type
	LockEv, LockState(..), LockId,
	-- * Handle
	handleLock ) where

import Control.Monad.State (StateT, get, modify)
import Data.Type.Set (Singleton)
import Data.OneOrMore hiding (merge)
import Data.Bool (bool)

import Control.Moffy.Handle
import Control.Moffy.Event.Lock.Internal

---------------------------------------------------------------------------

-- * LOCKID AND LOCKSTATE
-- * EVENT
--	+ NEWLOCKID
--	+ GETLOCK
--	+ UNLOCK
-- * HANDLE AND WITHLOCK

handleNewLockId ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton NewLockId)
handleNewLockId rqs = get >>= \s -> let i = getNextLockId s in
	Just (singleton $ OccNewLockId (LockId i) ti)
		<$ modify (`putNextLockId` (i + 1))
	where NewLockIdReq ti = extract rqs

handleGetLock ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton GetLock)
handleGetLock rqs = get >>= \s -> bool
	(Just (singleton $ OccGetLock l ti) <$ modify (`lockIt` l))
	(pure Nothing)
	(s `isLocked` l)
	where GetLockReq l ti _ = extract rqs

handleUnlock ::
	(LockState s, Monad m) => Handle' (StateT s m) (Singleton Unlock)
handleUnlock rqs = Just (singleton OccUnlock) <$ modify (`unlockIt` l)
	where UnlockReq l = extract rqs

---------------------------------------------------------------------------
-- HANDLE AND WITHLOCK
---------------------------------------------------------------------------

handleLock :: (LockState s, Monad m) => Handle' (StateT s m) LockEv
handleLock = handleNewLockId `merge` handleGetLock `merge` handleUnlock
