{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Count where

import Data.Or

import Moffy.React
import Moffy.Handle
import Moffy.Event.Mouse
import Moffy.XFieldHandle.Mouse
import Field

leftCount :: Int -> React s MouseEv Int
leftCount c = adjust (leftClick `first` rightClick) >>= \case
	L () -> leftCount $ c + 1
	R () -> pure c
	LR () () -> pure $ c + 1

tryLeftCount :: IO Int
tryLeftCount = do
	f <- openField "TRY LEFT COUNT" [buttonPressMask, exposureMask]
	interpretReact (retry $ handleMouse Nothing f) (leftCount 0) <* closeField f
