{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.StepByStepBoxNew where

import Data.Or

import Moffy.ReactNew
import Moffy.React.Common
import Moffy.Handle
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse
import Field

tryClickNew :: IO [MouseBtn]
tryClickNew = do
	f <- openField "TRY CLICK" [buttonPressMask, exposureMask]
	interpretReact (retry $ handleMouse Nothing f) (adjust mouseDown) <* closeField f

sameClickNew :: React s MouseEv Bool
sameClickNew = adjust $ (==) <$> mouseDown <*> mouseDown

trySameClickNew :: IO Bool
trySameClickNew = do
	f <- openField "TRY SAME CLICK" [buttonPressMask]
	interpretReact (retry $ handleMouse Nothing f) sameClickNew <* closeField f

leftDownRightUpNew :: React s MouseEv (Or () ())
leftDownRightUpNew = adjust $ leftClick `first` rightUp

tryLeftDownRightUpNew :: IO (Or () ())
tryLeftDownRightUpNew = do
	f <- openField "LEFT DOWN RIGHT UP" [buttonPressMask, buttonReleaseMask]
	interpretReact (retry $ handleMouse Nothing f) leftDownRightUpNew <* closeField f
