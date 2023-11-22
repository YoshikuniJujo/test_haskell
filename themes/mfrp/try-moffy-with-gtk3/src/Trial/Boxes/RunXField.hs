{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.RunXField (
	-- * runBoxes
	runBoxes ) where

import Control.Moffy.Event.Window
import Control.Moffy.Handle (
	HandleSt', retrySt, liftHandle', popInput, pushInput, beforeSt )
import Control.Moffy.Handle.Time (Mode(..), handleTimeEvPlus, TimeState(..))
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Viewable.Shape (Box)
import Control.Moffy.View.XField (drawBoxes)
import Control.Moffy.Run (interpretSt)
import Data.Time (DiffTime)
import Data.Time.Clock.TAI (AbsoluteTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)

import Trial.Boxes.BoxEv (SigB, BoxEv, BoxEvGen)
import Field (
	Field, openField, closeField,
	exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask )

import Control.Moffy.Event.Cursor
import Data.Type.Set

---------------------------------------------------------------------------

runBoxes :: String -> SigB s [Box] r -> IO r
runBoxes ttl s = do
	f <- openField ttl [
		exposureMask,
		buttonPressMask, buttonReleaseMask, pointerMotionMask ]
	(r, _) <- interpretSt (retrySt $ handleBoxes 0.05 f) (drawBoxes f) s
		. initialBoxesState . systemToTAITime =<< getSystemTime
	r <$ closeField f

initialBoxesState :: AbsoluteTime -> BoxesState
initialBoxesState t = BoxesState {
	bsMode = InitialMode,
	bsLatestTime = t,
	bsDefaultWindow = Nothing }

handleBoxes :: DiffTime -> Field -> HandleSt' BoxesState IO (CursorEv :+: BoxEv)
handleBoxes dt f = handleDefaultWindow `beforeSt` handleBoxesGen dt f

handleBoxesGen :: DiffTime -> Field -> HandleSt' BoxesState IO (CursorEv :+: BoxEvGen)
handleBoxesGen = curry . popInput . handleTimeEvPlus
	. pushInput . uncurry $ (liftHandle' .) . handle . Just

data BoxesState = BoxesState {
	bsMode :: Mode,
	bsLatestTime :: AbsoluteTime,
	bsDefaultWindow :: Maybe WindowId }
	deriving Show

instance TimeState BoxesState where
	getMode = bsMode; putMode s m = s { bsMode = m }
	getLatestTime = bsLatestTime; putLatestTime s lt = s { bsLatestTime = lt }

instance DefaultWindowState BoxesState where
	getDefaultWindow = bsDefaultWindow
	putDefaultWindow s dw = s { bsDefaultWindow = Just dw }
