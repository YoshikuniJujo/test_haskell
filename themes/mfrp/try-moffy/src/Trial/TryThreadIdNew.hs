{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryThreadIdNew where

import Data.Type.Set
import Data.Or

import Moffy.ReactNew
import Moffy.React.Common hiding (getThreadId)
import Moffy.Handle
import Moffy.EventHandle.ThreadId
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse
import Field

trySingleThreadId :: IO ThreadId
trySingleThreadId = interpretReact (retry $ handleGetThreadId) getThreadId

tryDoubleThreadId :: IO (Or ThreadId ThreadId)
tryDoubleThreadId = interpretReact (retry $ handleGetThreadId) $ getThreadId `first` getThreadId

leftRightThreadId :: React s (GetThreadId :- MouseEv) (Or ThreadId ThreadId)
leftRightThreadId =
	(adjust leftClick >> adjust getThreadId :: React s (GetThreadId :- MouseEv) ThreadId) `first`
	(adjust rightClick >> adjust getThreadId :: React s (GetThreadId :- MouseEv) ThreadId)

tryLeftRightThreadIdNew :: IO (Or ThreadId ThreadId)
tryLeftRightThreadIdNew = do
	f <- openField "TRY LEFT RIGHT THREAD ID" [buttonPressMask]
	interpretReact (retry $ handleGetThreadId `before` handleMouse Nothing f) leftRightThreadId
		<* closeField f
