{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Count (tryLeftCount, tryLeftCountSig, tryLeftRandomSig) where

import Control.Moffy (Sig, React, adjust, first, waitFor, emit)
import Control.Moffy.Event.Mouse (MouseEv, leftClick, rightClick)
import Control.Moffy.Handle (retry)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpret, interpretReact)
import Data.Or (Or(..))
import System.Random (StdGen, mkStdGen, random)

import Field (openField, closeField, exposureMask, buttonPressMask)

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

-- * TRIAL
-- * REACT AND SIG
-- * RUN

---------------------------------------------------------------------------
-- TRIAL
---------------------------------------------------------------------------

tryLeftCount :: IO Int
tryLeftCount = runMouseReact $ leftCount 0

tryLeftCountSig :: IO Int
tryLeftCountSig = runMouse $ leftCountSig 0

tryLeftRandomSig :: IO StdGen
tryLeftRandomSig = runMouse . leftRandomSig $ mkStdGen 8

---------------------------------------------------------------------------
-- REACT AND SIG
---------------------------------------------------------------------------

w0 :: WindowId
w0 = WindowId 0

leftCount :: Int -> React s MouseEv Int
leftCount c = adjust (leftClick w0 `first` rightClick w0) >>= \case
	L () -> leftCount $ c + 1; R () -> pure c; LR () () -> pure $ c + 1

leftCountSig :: Int -> Sig s MouseEv Int Int
leftCountSig c =
	emit c >> waitFor (adjust $ leftClick w0 `first` rightClick w0) >>= \case
		L () -> leftCountSig $ c + 1; R () -> pure c
		LR () () -> pure $ c + 1

leftRandomSig :: StdGen -> Sig s MouseEv Int StdGen
leftRandomSig (random -> (i, g')) =
	emit i >> waitFor (adjust $ leftClick (WindowId 0) `first` rightClick (WindowId 0)) >>= \case
		L () -> leftRandomSig g'; R () -> pure g'; LR () () -> pure g'

---------------------------------------------------------------------------
-- RUN
---------------------------------------------------------------------------

runMouseReact :: React s MouseEv r -> IO r
runMouseReact r = do
	f <- openField "RUN MOUSE REACT" [exposureMask, buttonPressMask]
	interpretReact (retry $ handle Nothing f) r <* closeField f

runMouse :: Show a => Sig s MouseEv a r -> IO r
runMouse s = do
	f <- openField "RUN MOUSE" [exposureMask, buttonPressMask]
	interpret (retry $ handle Nothing f) print s <* closeField f
