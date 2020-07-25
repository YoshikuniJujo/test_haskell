{-# LANGUAGE BlockArguments, LambdaCase #-}
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
import Control.Moffy.Handle.XField.Mouse
import Control.Moffy.Handle.XField
import Control.Monad.Freer.Par
import Field

import Data.Time
import Data.OneOrMore

handleMouse :: Maybe DiffTime -> Field -> Handle' IO (DeleteEvent :- MouseEv)
handleMouse mprd f rqs = handleWith (\case MouseEv e -> Just $ Data.OneOrMore.expand e; _ -> Nothing) mprd f rqs

checkParGetThreadId :: IO (Or ThreadId ThreadId)
checkParGetThreadId = interpretReact (retry handleGetThreadId) $ getThreadId `first` getThreadId

runSharingParGetThreadId :: IO (Or ThreadId ThreadId)
runSharingParGetThreadId = do
	f <- openField "RUN SHARING PAR GET THREADID" [buttonPressMask, exposureMask]
	r <- runUnique do
		gt <- tag (adjust leftClick >> adjust getThreadId :: React s (GetThreadId :- DeleteEvent :- MouseEv) ThreadId)
		pure $ interpretReact (retry $ handleGetThreadId `before` handleMouse (Just 0.05) f) $ gt `first` gt
	r <$ closeField f
