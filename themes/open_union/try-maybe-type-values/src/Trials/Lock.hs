{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Lock where

import Control.Monad.State
import Data.Type.Set
import Data.UnionSet
import Data.Bool
import MonadicFrp
import MonadicFrp.Handle
import MonadicFrp.ThreadId
import Trials.Followbox.ThreadId

type LockId = Int

class LockState s where
	isLocked :: s -> LockId -> Bool
	lockIt :: s -> LockId -> s
	unlockIt :: s -> LockId -> s

data GetLock = GetLockReq LockId ThreadId deriving (Show, Eq)
numbered 8 [t| GetLock |]
instance Mrgable GetLock where gl1 `mrg` _gl2 = gl1
instance Request GetLock where
	data Occurred GetLock = OccGetLock LockId ThreadId

getLock :: LockId -> React (GetThreadId :- GetLock :- 'Nil) ()
getLock l = adjust getThreadId >>= \ti ->
	bool (getLock l) (pure ()) =<< adjust (await (GetLockReq l ti)
		\(OccGetLock l' ti') -> l == l' && ti == ti')

handleGetLock :: (LockState s, Monad m) => Handle' (StateT s m) (Singleton GetLock)
handleGetLock reqs = get >>= \s ->
	bool (Just (singleton $ OccGetLock l ti) <$ modify (`lockIt` l)) (pure Nothing) $ s `isLocked` l
	where GetLockReq l ti = extract reqs

data Unlock = UnlockReq LockId deriving Show
numbered 8 [t| Unlock |]
instance Mrgable Unlock where ul1 `mrg` _ul2 = ul1
instance Request Unlock where data Occurred Unlock = OccUnlock

unlock :: LockId -> React (Singleton Unlock) ()
unlock l = await (UnlockReq l) (const ())

handleUnlock :: (LockState s, Monad m) => Handle' (StateT s m) (Singleton Unlock)
handleUnlock reqs = Just (singleton OccUnlock) <$ modify (`unlockIt` l)
	where UnlockReq l = extract reqs

-- withLock :: 
