{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.TryReact (
	tryLeftClick, trySameClick, trySleep, tryDoubler, tryFirstPoint ) where

import Control.Monad.State (runStateT)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime)

import TryMyInterface.Boxes (leftClick, sameClick, doubler, firstPoint)
import TryMyInterface.Boxes.Handlers (handle, handleWithoutTime)
import TryMyInterface.Boxes.Events (ReactG, sleep)
import MonadicFrp.MyInterface (interpret, adjust)
import Field (Field, openField, closeField, exposureMask, buttonPressMask)

withField :: String -> (Field -> IO a) -> IO a
withField fn act = (<*) <$> act <*> closeField
	=<< openField fn [exposureMask, buttonPressMask]

tryLeftClick :: IO ()
tryLeftClick = withField "tryLeftClick" \f ->
	interpret (handleWithoutTime f) (adjust leftClick :: ReactG ())

trySameClick :: IO ()
trySameClick = withField "trySameClick" \f ->
	print =<< interpret (handleWithoutTime f) sameClick

trySleep :: IO ()
trySleep = withField "trySleep" \f -> do
	now <- getTAITime
	print =<< interpret (handle 0.5 f) (adjust $ sleep 3) `runStateT` now

tryDoubler :: IO ()
tryDoubler = withField "tryDoubler" \f -> do
	now <- getTAITime
	print =<< interpret (handle 0.05 f) doubler `runStateT` now

tryFirstPoint :: IO ()
tryFirstPoint = withField "tryFirstPoint" \f -> do
	now <- getTAITime
	print =<< interpret (handle 0.05 f) firstPoint `runStateT` now

getTAITime :: IO AbsoluteTime
getTAITime = systemToTAITime <$> getSystemTime
