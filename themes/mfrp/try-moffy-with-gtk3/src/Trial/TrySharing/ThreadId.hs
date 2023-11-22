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
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (MouseEv, MouseBtn, mouseDown)
import Control.Moffy.Handle (retry, before, liftHandle', retrySt, beforeSt, mergeSt)
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.ThreadId (handleGetThreadId)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretReact, interpretReactSt)
import Control.Concurrent (threadDelay)
import Data.Type.Set ((:-))
import Data.Or (Or)
import System.IO.Unsafe (unsafePerformIO)

import Field (openField, closeField, exposureMask, buttonPressMask)

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

-- * TRIAL
-- * PARTS

---------------------------------------------------------------------------
-- TRIAL
---------------------------------------------------------------------------

runFirstGetThreadId :: IO (Or' (MouseBtn, ThreadId), Maybe WindowId)
runFirstGetThreadId = runMouseThreadId $ r `first` r
	where r = (,) <$> heavyMouseDown <*> heavyGetThreadId

runSharingFirstGetThreadId :: IO (Or' (MouseBtn, ThreadId), Maybe WindowId)
runSharingFirstGetThreadId = runTagged do
	r <- tag $ (,) <$> heavyMouseDown <*> heavyGetThreadId
	pure $ runMouseThreadId $ r `first` r

runSharingFirstGetThreadId' :: IO (Or' (MouseBtn, ThreadId), Maybe WindowId)
runSharingFirstGetThreadId' = runTagged do
	r <- tag $ (,) <$> heavyMouseDown <*> heavyGetThreadId
	pure $ runMouseThreadId $ r `first'` r

---------------------------------------------------------------------------
-- PARTS
---------------------------------------------------------------------------

type Or' a = Or a a

runMouseThreadId :: React s (LoadDefaultWindow :- GetThreadId :- MouseEv) r -> IO (r, Maybe WindowId)
runMouseThreadId r = do
	f <- openField "RUN MOUSE THREAD ID" [buttonPressMask, exposureMask]
	(<* closeField f) $ interpretReactSt
		(retrySt $
			handleDefaultWindow `mergeSt`
			liftHandle' handleGetThreadId `beforeSt` liftHandle' (handle (Just 0.05) f)) r Nothing

heavyGetThreadId :: React s (LoadDefaultWindow :- GetThreadId :- MouseEv) ThreadId
heavyGetThreadId = adjust $ heavyId "getThreadId" <$> getThreadId

heavyMouseDown :: React s (LoadDefaultWindow :- GetThreadId :- MouseEv) MouseBtn
heavyMouseDown = adjust $ heavyId "mouseDown" <$> mouseDown

heavyId :: String -> a -> a
heavyId s x = unsafePerformIO $ x <$
	(msg (s ++ ": BEGIN") >> threadDelay 2000000 >> msg (s ++ ": END"))
	where msg = putStrLn . ("\n" ++)
