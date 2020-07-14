{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Lock (
	-- * Type
	LockEv, LockState(..), LockId,
	-- * Event
	GetThreadIdNewLockId, newLockId,
	GetThreadIdGetLock, SingletonUnlock, withLock ) where

import Control.Moffy.Event.Lock.Internal (
	LockEv, LockState(..), LockId,
	GetThreadIdNewLockId, newLockId,
	GetThreadIdGetLock, SingletonUnlock, withLock )
