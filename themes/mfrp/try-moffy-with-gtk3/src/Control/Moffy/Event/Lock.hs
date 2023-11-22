{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Lock (
	-- * Type
	LockEv, GetThreadIdNewLockId, GetThreadIdGetLock, SingletonUnlock,
	LockId,
	-- * Event
	newLockId, withLock, withLockSig ) where

import Control.Moffy.Event.Lock.Internal (
	LockEv, GetThreadIdNewLockId, GetThreadIdGetLock, SingletonUnlock,
	LockId, newLockId, withLock, withLockSig )
