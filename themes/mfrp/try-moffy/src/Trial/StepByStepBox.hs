{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox where

import Data.Or

import Moffy.React
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse

import Field

tryClick :: IO [MouseBtn]
tryClick = do
	f <- openField "TRY CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse f) (adjust mouseDown) <* closeField f

sameClick :: React s MouseEv Bool
sameClick = adjust do
	pressed1 <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed1 == pressed2

trySameClick :: IO Bool
trySameClick = do
	f <- openField "TRY SAME CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse f) sameClick <* closeField f

leftOrRightClick :: React s MouseEv (Or () ())
leftOrRightClick = adjust $ leftClick `first` rightClick

tryLeftOrRightClick :: IO (Or () ())
tryLeftOrRightClick = do
	f <- openField "LEFT OR RIGHT CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse f) leftOrRightClick <* closeField f
