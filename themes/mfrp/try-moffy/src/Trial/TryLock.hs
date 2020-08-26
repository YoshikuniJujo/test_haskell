{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryLock (
	-- * Type
	LockSt,
	-- * Action
	trySingleLeftCount, tryNoLockLeftCount2, tryLockLeftCount2 ) where

import Control.Monad (void)
import Control.Moffy (Sig, adjust, emit, waitFor, first)
import Control.Moffy.Event.ThreadId (GetThreadId)
import Control.Moffy.Event.Lock (LockEv, LockId, newLockId, withLock)
import Control.Moffy.Event.Mouse (MouseDown, leftClick, rightClick)
import Control.Moffy.Handle (retry, liftSt, retrySt, mergeSt)
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.Lock (LockState(..), handleLock)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpret, interpretSt)
import Data.Type.Set (Singleton, (:-))
import Data.Type.Flip ((<$%>), (<*%>))
import Data.Or (Or(..))
import Data.List (delete)

import Field (openField, closeField, exposureMask, buttonPressMask)

---------------------------------------------------------------------------

-- * LOCK ST
-- * TRIAl
--	+ SINGLE
--	+ NO LOCK
--	+ LOCK
-- * RUN

---------------------------------------------------------------------------
-- LOCK ST
---------------------------------------------------------------------------

data LockSt = LockSt { nextLockId :: Int, lockState :: [LockId] } deriving Show

instance LockState LockSt where
	getNextLockId = nextLockId
	putNextLockId s li = s { nextLockId = li }
	isLocked LockSt { lockState = ls } li = li `elem` ls
	lockIt s li = s { lockState = li : lockState s }
	unlockIt s li = s { lockState = delete li (lockState s) }

---------------------------------------------------------------------------
-- TRIAL
---------------------------------------------------------------------------

-- SINGLE

trySingleLeftCount :: IO Int
trySingleLeftCount = runClick $ leftCount 0

leftCount :: Int -> Sig s (Singleton MouseDown) Int Int
leftCount c = do
	emit c
	waitFor (leftClick `first` rightClick) >>= \case
		L () -> leftCount $ c + 1
		R () -> pure c
		LR () () -> pure $ c + 1

-- NO LOCK

tryNoLockLeftCount2 :: IO ()
tryNoLockLeftCount2 =
	runClick $ (,) <$%> void (leftCount 0) <*%> void (leftCount 0)

-- LOCK

tryLockLeftCount2 :: IO ((), LockSt)
tryLockLeftCount2 = runClickLockSt  do
	l <- waitFor $ adjust newLockId
	(,) <$%> void (lockLeftCount l 0) <*%> void (lockLeftCount l 0)

lockLeftCount :: LockId -> Int -> Sig s (MouseDown :- GetThreadId :- LockEv) Int Int
lockLeftCount l c = do
	emit c
	waitFor (withLock l $ leftClick `first` rightClick) >>= \case
		L () -> lockLeftCount l $ c + 1
		R () -> pure c
		LR () () -> pure $ c + 1

---------------------------------------------------------------------------
-- RUN
---------------------------------------------------------------------------

runClick :: Show a => Sig s (Singleton MouseDown) a r -> IO r
runClick s = do
	f <- openField "TRY LEFT COUNT" [buttonPressMask, exposureMask]
	interpret (retry $ handle Nothing f) print s  <* closeField f

runClickLockSt :: Show a => Sig s (MouseDown :- GetThreadId :- LockEv) a r -> IO (r, LockSt)
runClickLockSt s = do
	f <- openField "TRY LOCK LEFT COUNT 2" [buttonPressMask, exposureMask]
	interpretSt (retrySt $ handleGetThreadId' `mergeSt` handleLock `mergeSt` handle' f) print s (LockSt 0 [])
		<* closeField f
	where
	handle' f = liftSt . handle Nothing f
	handleGetThreadId' = liftSt . handleGetThreadId
