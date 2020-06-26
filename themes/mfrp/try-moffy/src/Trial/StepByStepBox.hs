{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox where

import Data.Type.Set
import Moffy.React
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse

import Field

tryClick :: IO [MouseBtn]
tryClick = do
	f <- openField "TRY CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse f) mouseDown <* closeField f

sameClick :: React s (Singleton MouseDown) Bool
sameClick = do
	pressed1 <- mouseDown
	pressed2 <- mouseDown
	pure $ pressed1 == pressed2

trySameClick :: IO Bool
trySameClick = do
	f <- openField "TRY SAME CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse f) sameClick <* closeField f
