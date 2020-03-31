{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.TryReact where

import Control.Monad.State
import Data.Time.Clock.System

import MonadicFrp.MyInterface
import TryMyInterface.Boxes
import TryMyInterface.Boxes.Events
import TryMyInterface.Boxes.Handlers
import Field

withField :: String -> (Field -> IO a) -> IO a
withField fn act = do
	f <- openField fn [exposureMask, buttonPressMask]
	act f <* closeField f

tryLeftClick :: IO ()
tryLeftClick = withField "tryLeftClick" \f ->
	interpret (handleWithoutTime f) (adjust leftClick :: ReactG ())

trySameClick :: IO ()
trySameClick = withField "trySameClick" \f ->
	interpret (handleWithoutTime f) sameClick >>= print

trySleep :: IO ()
trySleep = withField "trySleep" \f -> do
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.5 f) (adjust $ sleep 3) `runStateT` now >>= print
