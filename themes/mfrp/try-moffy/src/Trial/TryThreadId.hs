module Trial.TryThreadId where

import Data.Or

import Moffy.React hiding (getThreadId)
import Moffy.EventHandle.ThreadId

trySingleThreadId :: IO ThreadId
trySingleThreadId = interpretReact (retry $ handleGetThreadId) getThreadId

tryDoubleThreadId :: IO (Or ThreadId ThreadId)
tryDoubleThreadId = interpretReact (retry $ handleGetThreadId) $ getThreadId `first` getThreadId
