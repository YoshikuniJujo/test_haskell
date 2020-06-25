{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBox where

import Moffy.React
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse

import Field

tryClick :: IO [MouseBtn]
tryClick = do
	f <- openField "TRY CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse f) mouseDown <* closeField f
