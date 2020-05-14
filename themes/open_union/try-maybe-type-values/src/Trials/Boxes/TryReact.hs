{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.TryReact (
	tryLeftClick, trySameClick, trySleep, tryDoubler, tryFirstPoint ) where

import Control.Monad.State (runStateT)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime)

import Trials.Boxes (leftClick, sameClick, doubler, firstPoint)
import Trials.Boxes.Handle (handle, AB(..), handleWithoutTime)
import Trials.Boxes.Event (ReactG, sleep)
import MonadicFrp (adjust)
import MonadicFrp.Run (interpretReact, interpretReactSt)
import Field (Field, openField, closeField, exposureMask, buttonPressMask)

withField :: String -> (Field -> IO a) -> IO a
withField fn act = (<*) <$> act <*> closeField
	=<< openField fn [exposureMask, buttonPressMask]

tryLeftClick :: IO ()
tryLeftClick = withField "tryLeftClick" \f ->
	interpretReact (handleWithoutTime f) (adjust leftClick :: ReactG ())

trySameClick :: IO ()
trySameClick = withField "trySameClick" \f ->
	print =<< interpretReact (handleWithoutTime f) sameClick

trySleep :: IO ()
trySleep = withField "trySleep" \f -> do
	now <- getTAITime
	print =<< interpretReactSt A (handle 0.5 f) (adjust $ sleep 3) `runStateT` now

tryDoubler :: IO ()
tryDoubler = withField "tryDoubler" \f -> do
	now <- getTAITime
	print =<< interpretReactSt A (handle 0.05 f) doubler `runStateT` now

tryFirstPoint :: IO ()
tryFirstPoint = withField "tryFirstPoint" \f -> do
	now <- getTAITime
	print =<< interpretReactSt A (handle 0.05 f) firstPoint `runStateT` now

getTAITime :: IO AbsoluteTime
getTAITime = systemToTAITime <$> getSystemTime
