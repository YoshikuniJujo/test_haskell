{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Lock (
	-- * Type
	LockEv, LockId,
	-- * Event
	GetThreadIdNewLockId, newLockId,
	GetThreadIdGetLock, SingletonUnlock, withLock ) where

import Control.Moffy.Event.Lock.Internal (
	LockEv, LockId,
	GetThreadIdNewLockId, newLockId,
	GetThreadIdGetLock, SingletonUnlock, withLock )
