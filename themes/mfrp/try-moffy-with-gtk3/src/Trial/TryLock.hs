{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
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
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (MouseDown, leftClick, rightClick)
import Control.Moffy.Handle (liftSt, retrySt, mergeSt, liftHandle')
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.Lock (LockState(..), handleLock)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretSt)
import Data.Type.Set ((:-), pattern Nil)
import Data.Type.Flip ((<$%>), (<*%>))
import Data.Or (Or(..))
import Data.List (delete)

import Field (openField, closeField, exposureMask, buttonPressMask)

import Control.Moffy.Event.Window

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

data LockSt = LockSt {
	nextLockId :: Int, lockState :: [LockId], lsDefaultWindow :: Maybe WindowId }
	deriving Show

instance LockState LockSt where
	getNextLockId = nextLockId; putNextLockId s li = s { nextLockId = li }
	isLocked LockSt { lockState = ls } li = li `elem` ls
	lockIt s li = s { lockState = li : lockState s }
	unlockIt s li = s { lockState = delete li $ lockState s }

instance DefaultWindowState LockSt where
	getDefaultWindow = lsDefaultWindow
	putDefaultWindow s dw = s { lsDefaultWindow = Just dw }

---------------------------------------------------------------------------
-- TRIAL
---------------------------------------------------------------------------

-- SINGLE

trySingleLeftCount :: IO (Int, Maybe WindowId)
trySingleLeftCount = runClick $ leftCount 0

leftCount :: Int -> Sig s (LoadDefaultWindow :- MouseDown :- 'Nil) Int Int
leftCount c = emit c >> waitFor (leftClick `first` rightClick) >>= \case
	L () -> leftCount $ c + 1; R () -> pure c; LR () () -> pure $ c + 1

-- NO LOCK

tryNoLockLeftCount2 :: IO ((), Maybe WindowId)
tryNoLockLeftCount2 =
	runClick $ (,) <$%> void (leftCount 0) <*%> void (leftCount 0)

-- LOCK

tryLockLeftCount2 :: IO ((), LockSt)
tryLockLeftCount2 = runClickLockSt  do
	l <- waitFor $ adjust newLockId
	(,) <$%> void (lockLeftCount l 0) <*%> void (lockLeftCount l 0)

lockLeftCount ::
	LockId -> Int -> Sig s (LoadDefaultWindow :- MouseDown :- GetThreadId :- LockEv) Int Int
lockLeftCount l c =
	emit c >> waitFor (withLock l $ leftClick `first` rightClick) >>= \case
		L () -> lockLeftCount l $ c + 1; R () -> pure c
		LR () () -> pure $ c + 1

---------------------------------------------------------------------------
-- RUN
---------------------------------------------------------------------------

runClick :: Show a => Sig s (LoadDefaultWindow :- MouseDown :- 'Nil) a r -> IO (r, Maybe WindowId)
runClick s = do
	f <- openField "TRY LEFT COUNT" [buttonPressMask, exposureMask]
	interpretSt
		(retrySt $ handleDefaultWindow `mergeSt` liftHandle' (handle Nothing f))
		print s Nothing <* closeField f

runClickLockSt :: Show a =>
	Sig s (LoadDefaultWindow :- MouseDown :- GetThreadId :- LockEv) a r -> IO (r, LockSt)
runClickLockSt s = do
	f <- openField "TRY LOCK LEFT COUNT 2" [buttonPressMask, exposureMask]
	interpretSt (hdl f) print s (LockSt 0 [] Nothing) <* closeField f
	where hdl f = retrySt $
		handleDefaultWindow `mergeSt`
		liftSt . handleGetThreadId `mergeSt` handleLock `mergeSt`
		liftSt . handle Nothing f
