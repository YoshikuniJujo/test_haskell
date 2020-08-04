{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryLock where

import Control.Monad
import Control.Monad.State
import Control.Moffy
import Control.Moffy.Event.ThreadId
import Control.Moffy.Event.Lock
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle
import Control.Moffy.Handle.ThreadId
import Control.Moffy.Handle.Lock
import Control.Moffy.Handle.XField
import Control.Moffy.Run
import Data.Type.Set
import Data.Type.Flip
import Data.Or
import Data.List
import Field

leftCount :: Int -> Sig s (Singleton MouseDown) Int Int
leftCount c = do
	emit c
	waitFor (leftClick `first` rightClick) >>= \case
		L () -> leftCount $ c + 1
		R () -> pure c
		LR () () -> pure $ c + 1

tryLeftCount :: IO Int
tryLeftCount = do
	f <- openField "TRY LEFT COUNT" [buttonPressMask, exposureMask]
	interpret (retry $ handle Nothing f) print (leftCount 0) <* closeField f

noLockLeftCount2 :: Sig s (Singleton MouseDown) (Int, Int) ()
noLockLeftCount2 = (,) <$%> void (leftCount 0) <*%> void (leftCount 0)

tryNoLockLeftCount2 :: IO ()
tryNoLockLeftCount2 = do
	f <- openField "TRY NO LOCK LEFT COUNT 2" [buttonPressMask, exposureMask]
	interpret (retry $ handle Nothing f) print noLockLeftCount2 <* closeField f

lockLeftCount :: LockId -> Int -> Sig s (MouseDown :- GetThreadId :- LockEv) Int Int
lockLeftCount l c = do
	emit c
	waitFor (withLock l $ leftClick `first` rightClick) >>= \case
		L () -> lockLeftCount l $ c + 1
		R () -> pure c
		LR () () -> pure $ c + 1

lockLeftCount2 :: Sig s (MouseDown :- GetThreadId :- LockEv) (Int, Int) ()
lockLeftCount2 = do
	l <- waitFor $ adjust newLockId
	(,) <$%> void (lockLeftCount l 0) <*%> void (lockLeftCount l 0)

data LockSt = LockSt { nextLockId :: Int, lockState :: [LockId] } deriving Show

instance LockState LockSt where
	getNextLockId = nextLockId
	putNextLockId s li = s { nextLockId = li }
	isLocked LockSt { lockState = ls } li = li `elem` ls
	lockIt s li = s { lockState = li : lockState s }
	unlockIt s li = s { lockState = delete li (lockState s) }

tryLockLeftCount2 :: IO ((), LockSt)
tryLockLeftCount2 = do
	f <- openField "TRY LOCK LEFT COUNT 2" [buttonPressMask, exposureMask]
	interpret (retry $ handleGetThreadId `merge` handleLock `before` liftIO . handle Nothing f) (liftIO . print) lockLeftCount2 `runStateT` LockSt 0 []
		<* closeField f

mergeSt' :: (
	Monad m, ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleSt' st st m es -> HandleSt' st st m es' -> HandleSt' st st m (es :+: es')
mergeSt' h1 h2 = mergeSt h1 pure h2 pure

handle' :: LockState s => Field -> HandleSt' s s IO GuiEv
handle' f = liftSt . handle Nothing f

handleGetThreadId' :: LockState s => HandleSt' s s IO (Singleton GetThreadId)
handleGetThreadId' = liftSt . handleGetThreadId

tryLockLeftCount2' :: IO ((), LockSt)
tryLockLeftCount2' = do
	f <- openField "TRY LOCK LEFT COUNT 2" [buttonPressMask, exposureMask]
	interpretSt (retrySt $ handleGetThreadId' `mergeSt'` handleLock' `mergeSt'` handle' f) print lockLeftCount2 (LockSt 0 [])
		<* closeField f
