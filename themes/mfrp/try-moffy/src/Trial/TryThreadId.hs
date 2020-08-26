{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryThreadId (
	trySingleThreadId, tryDoubleThreadId, tryLeftRightThreadId, tryLeftRightThreadId' ) where

import Control.Moffy (React, adjust, first)
import Control.Moffy.NoThreadId (first')
import Control.Moffy.Event.ThreadId (GetThreadId, ThreadId, getThreadId)
import Control.Moffy.Event.Mouse (MouseEv, leftClick, rightClick)
import Control.Moffy.Handle (retry, before)
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretReact)
import Data.Type.Set (Singleton, (:-))
import Data.Or (Or)

import Field (openField, closeField, exposureMask, buttonPressMask)

---------------------------------------------------------------------------

trySingleThreadId :: IO ThreadId
trySingleThreadId = runGetThreadId getThreadId

tryDoubleThreadId :: IO (Or ThreadId ThreadId)
tryDoubleThreadId = runGetThreadId $ getThreadId `first` getThreadId

runGetThreadId :: React s (Singleton GetThreadId) a -> IO a
runGetThreadId = interpretReact $ retry handleGetThreadId

tryLeftRightThreadId :: IO (Or ThreadId ThreadId)
tryLeftRightThreadId = runMouseGetThreadId $
	clickThenGetThreadId (adjust leftClick) `first`
	clickThenGetThreadId (adjust rightClick)

tryLeftRightThreadId' :: IO (Or ThreadId ThreadId)
tryLeftRightThreadId' = runMouseGetThreadId $
	clickThenGetThreadId (adjust leftClick) `first'`
	clickThenGetThreadId (adjust rightClick)

runMouseGetThreadId :: React s (GetThreadId :- MouseEv) a -> IO a
runMouseGetThreadId r = do
	f <- openField "RUN MOUSE GET THREAD ID" [exposureMask, buttonPressMask]
	interpretReact (retry $ handleGetThreadId `before` handle Nothing f) r
		<* closeField f

clickThenGetThreadId ::
	React s MouseEv r -> React s (GetThreadId :- MouseEv) ThreadId
clickThenGetThreadId c = adjust c >> adjust getThreadId
