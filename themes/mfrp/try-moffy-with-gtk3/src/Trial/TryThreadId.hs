{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryThreadId (
	trySingleThreadId, tryDoubleThreadId, tryLeftRightThreadId, tryLeftRightThreadId' ) where

import Control.Moffy (React, adjust, first)
import Control.Moffy.NoThreadId (first')
import Control.Moffy.Event.ThreadId (GetThreadId, ThreadId, getThreadId)
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (MouseEv, leftClick, rightClick)
import Control.Moffy.Handle (retry, before, liftHandle', retrySt, mergeSt, beforeSt)
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretReact, interpretReactSt)
import Data.Type.Set (Singleton, (:-))
import Data.Or (Or)

import Field (openField, closeField, exposureMask, buttonPressMask)

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

trySingleThreadId :: IO (ThreadId, Maybe WindowId)
trySingleThreadId = runGetThreadId getThreadId

tryDoubleThreadId :: IO (Or ThreadId ThreadId, Maybe WindowId)
tryDoubleThreadId = runGetThreadId $ getThreadId `first` getThreadId

runGetThreadId :: React s (Singleton GetThreadId) a -> IO (a, Maybe WindowId)
runGetThreadId r = interpretReactSt (retrySt $ handleDefaultWindow `beforeSt` liftHandle' handleGetThreadId) r Nothing

tryLeftRightThreadId :: IO (Or ThreadId ThreadId, Maybe WindowId)
tryLeftRightThreadId = runMouseGetThreadId $
	clickThenGetThreadId (adjust leftClick) `first`
	clickThenGetThreadId (adjust rightClick)

tryLeftRightThreadId' :: IO (Or ThreadId ThreadId, Maybe WindowId)
tryLeftRightThreadId' = runMouseGetThreadId $
	clickThenGetThreadId (adjust leftClick) `first'`
	clickThenGetThreadId (adjust rightClick)

runMouseGetThreadId :: React s (LoadDefaultWindow :- GetThreadId :- MouseEv) a -> IO (a, Maybe WindowId)
runMouseGetThreadId r = do
	f <- openField "RUN MOUSE GET THREAD ID" [exposureMask, buttonPressMask]
	interpretReactSt (retrySt $ handleDefaultWindow `mergeSt` liftHandle' handleGetThreadId `beforeSt` liftHandle' (handle Nothing f)) r Nothing
		<* closeField f

clickThenGetThreadId ::
	React s (LoadDefaultWindow :- MouseEv) r -> React s (LoadDefaultWindow :- GetThreadId :- MouseEv) ThreadId
clickThenGetThreadId c = adjust c >> adjust getThreadId
