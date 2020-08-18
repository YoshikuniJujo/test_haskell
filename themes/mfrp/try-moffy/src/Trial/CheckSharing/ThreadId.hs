{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckSharing.ThreadId where

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Type.Set
import Data.Or

import Control.Moffy.Handle.ThreadId
import Control.Moffy.Event.ThreadId

import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Delete
import Control.Moffy.Handle.XField
import Control.Monad.Freer.Par
import Field

checkParGetThreadId :: IO (Or ThreadId ThreadId)
checkParGetThreadId = interpretReact (retry handleGetThreadId) $ getThreadId `first` getThreadId

runSharingParGetThreadId :: IO (Or ThreadId ThreadId)
runSharingParGetThreadId = do
	f <- openField "RUN SHARING PAR GET THREADID" [buttonPressMask, exposureMask]
	r <- runTagged do
		gt <- tag (adjust leftClick >> adjust getThreadId :: React s (GetThreadId :- DeleteEvent :- MouseEv) ThreadId)
		pure $ interpretReact (retry $ handleGetThreadId `before` handle (Just 0.05) f) $ gt `first` gt
	r <$ closeField f
