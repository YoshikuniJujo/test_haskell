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
import Control.Moffy.Handle (HandleIo', mergeSt)
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
-- NEW HANDLE
---------------------------------------------------------------------------

handleLock :: (Monad m, LockState s) => HandleIo' s s m LockEv
handleLock = handleNewLockId' `mergeSt` handleGetLock' `mergeSt` handleUnlock'

handleNewLockId' ::
	(Applicative m, LockState s) => HandleIo' s s m (Singleton NewLockId)
handleNewLockId' (Singleton (NewLockIdReq t)) s = let i = getNextLockId s in
	pure (	Just . Singleton $ OccNewLockId (LockId i) t,
		s `putNextLockId` (i + 1) )

handleGetLock' ::
	(Applicative m, LockState s) => HandleIo' s s m (Singleton GetLock)
handleGetLock' (Singleton (GetLockReq l t _)) s = pure $ bool
	(Just (Singleton $ OccGetLock l t), s `lockIt` l) (Nothing, s)
	(s `isLocked` l)

handleUnlock' ::
	(Applicative m, LockState s) => HandleIo' s s m (Singleton Unlock)
handleUnlock' (Singleton (UnlockReq l)) s =
	pure (Just $ Singleton OccUnlock, s `unlockIt` l)
