{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryThreadId where

import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Handle
import Data.Type.Set
import Data.Or

import Moffy.EventHandle.ThreadId
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle.XField.Mouse
import Field

trySingleThreadId :: IO ThreadId
trySingleThreadId = interpretReact (retry handleGetThreadId) getThreadId

tryDoubleThreadId :: IO (Or ThreadId ThreadId)
tryDoubleThreadId = interpretReact (retry handleGetThreadId) $ getThreadId `first` getThreadId

leftRightThreadId :: React s (GetThreadId :- MouseEv) (Or ThreadId ThreadId)
leftRightThreadId =
	(adjust leftClick >> adjust getThreadId :: React s (GetThreadId :- MouseEv) ThreadId) `first`
	(adjust rightClick >> adjust getThreadId :: React s (GetThreadId :- MouseEv) ThreadId)

tryLeftRightThreadId :: IO (Or ThreadId ThreadId)
tryLeftRightThreadId = do
	f <- openField "TRY LEFT RIGHT THREAD ID" [buttonPressMask]
	interpretReact (retry $ handleGetThreadId `before` handleMouse Nothing f) leftRightThreadId
		<* closeField f
