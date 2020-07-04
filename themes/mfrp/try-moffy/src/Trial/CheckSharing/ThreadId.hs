{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckSharing.ThreadId where

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Type.Set
import Data.Or

import Moffy.EventHandle.ThreadId

import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse
import Freer
import Field

checkParGetThreadId :: IO (Or ThreadId ThreadId)
checkParGetThreadId = interpretReact (retry handleGetThreadId) $ getThreadId `first` getThreadId

runSharingParGetThreadId :: IO (Or ThreadId ThreadId)
runSharingParGetThreadId = do
	f <- openField "RUN SHARING PAR GET THREADID" [buttonPressMask, exposureMask]
	r <- runCount do
		gt <- addTag (adjust leftClick >> adjust getThreadId :: React s (GetThreadId :- MouseEv) ThreadId)
		pure $ interpretReact (retry $ handleGetThreadId `before` handleMouse (Just 0.05) f) $ gt `first` gt
	r <$ closeField f
