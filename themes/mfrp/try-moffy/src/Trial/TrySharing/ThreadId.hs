{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TrySharing.ThreadId (
	runFirstGetThreadId,
	runSharingFirstGetThreadId, runSharingFirstGetThreadId' ) where

import Control.Monad.Freer.Par (runTagged, tag)
import Control.Moffy (React, adjust, first)
import Control.Moffy.NoThreadId (first')
import Control.Moffy.Event.ThreadId (GetThreadId, ThreadId, getThreadId)
import Control.Moffy.Event.Mouse (MouseEv, MouseBtn, mouseDown)
import Control.Moffy.Handle (retry, before)
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretReact)
import Control.Concurrent (threadDelay)
import Data.Type.Set ((:-))
import Data.Or (Or)
import System.IO.Unsafe (unsafePerformIO)

import Field (openField, closeField, exposureMask, buttonPressMask)

---------------------------------------------------------------------------

-- * TRIAL
-- * PARTS

---------------------------------------------------------------------------
-- TRIAL
---------------------------------------------------------------------------

runFirstGetThreadId :: IO (Or' (MouseBtn, ThreadId))
runFirstGetThreadId = runMouseThreadId $ r `first` r
	where r = (,) <$> heavyMouseDown <*> heavyGetThreadId

runSharingFirstGetThreadId :: IO (Or' (MouseBtn, ThreadId))
runSharingFirstGetThreadId = runTagged do
	r <- tag $ (,) <$> heavyMouseDown <*> heavyGetThreadId
	pure $ runMouseThreadId $ r `first` r

runSharingFirstGetThreadId' :: IO (Or' (MouseBtn, ThreadId))
runSharingFirstGetThreadId' = runTagged do
	r <- tag $ (,) <$> heavyMouseDown <*> heavyGetThreadId
	pure $ runMouseThreadId $ r `first'` r

---------------------------------------------------------------------------
-- PARTS
---------------------------------------------------------------------------

type Or' a = Or a a

runMouseThreadId :: React s (GetThreadId :- MouseEv) r -> IO r
runMouseThreadId r = do
	f <- openField "RUN MOUSE THREAD ID" [buttonPressMask, exposureMask]
	(<* closeField f) $ interpretReact
		(retry $ handleGetThreadId `before` handle (Just 0.05) f) r

heavyGetThreadId :: React s (GetThreadId :- MouseEv) ThreadId
heavyGetThreadId = adjust $ heavyId "getThreadId" <$> getThreadId

heavyMouseDown :: React s (GetThreadId :- MouseEv) MouseBtn
heavyMouseDown = adjust $ heavyId "mouseDown" <$> mouseDown

heavyId :: String -> a -> a
heavyId s x = unsafePerformIO $ x <$
	(msg (s ++ ": BEGIN") >> threadDelay 2000000 >> msg (s ++ ": END"))
	where msg = putStrLn . ("\n" ++)
