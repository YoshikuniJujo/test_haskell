{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Count (tryLeftCount, tryLeftCountSig, tryLeftRandomSig) where

import Control.Moffy (Sig, React, adjust, first, waitFor, emit)
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Mouse.DefaultWindow (MouseEv, leftClick, rightClick)
import Control.Moffy.Handle (retrySt, beforeSt, liftHandle')
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretSt, interpretReactSt)
import Data.Type.Set
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

tryLeftCount :: IO (Int, Maybe WindowId)
tryLeftCount = runMouseReact $ leftCount 0

tryLeftCountSig :: IO (Int, Maybe WindowId)
tryLeftCountSig = runMouse $ leftCountSig 0

tryLeftRandomSig :: IO (StdGen, Maybe WindowId)
tryLeftRandomSig = runMouse . leftRandomSig $ mkStdGen 8

---------------------------------------------------------------------------
-- REACT AND SIG
---------------------------------------------------------------------------

leftCount :: Int -> React s (WindowNew :- DefaultWindowEv :+: MouseEv) Int
leftCount c = adjust (leftClick `first` rightClick) >>= \case
	L () -> leftCount $ c + 1; R () -> pure c; LR () () -> pure $ c + 1

leftCountSig :: Int -> Sig s (WindowNew :- DefaultWindowEv :+: MouseEv) Int Int
leftCountSig c =
	emit c >> waitFor (adjust $ leftClick `first` rightClick) >>= \case
		L () -> leftCountSig $ c + 1; R () -> pure c
		LR () () -> pure $ c + 1

leftRandomSig :: StdGen -> Sig s (WindowNew :- DefaultWindowEv :+: MouseEv) Int StdGen
leftRandomSig (random -> (i, g')) =
	emit i >> waitFor (adjust $ leftClick `first` rightClick) >>= \case
		L () -> leftRandomSig g'; R () -> pure g'; LR () () -> pure g'

---------------------------------------------------------------------------
-- RUN
---------------------------------------------------------------------------

runMouseReact :: React s (WindowNew :- DefaultWindowEv :+: MouseEv) r -> IO (r, Maybe WindowId)
runMouseReact r = do
	f <- openField "RUN MOUSE REACT" [exposureMask, buttonPressMask]
	interpretReactSt (retrySt $ handleDefaultWindow `beforeSt` liftHandle' (handle Nothing f))
		(adjust windowNew >>= adjust . storeDefaultWindow >> r)
		Nothing <* closeField f

runMouse :: Show a => Sig s (WindowNew :- DefaultWindowEv :+: MouseEv) a r -> IO (r, Maybe WindowId)
runMouse s = do
	f <- openField "RUN MOUSE" [exposureMask, buttonPressMask]
	interpretSt (retrySt $ handleDefaultWindow `beforeSt` liftHandle' (handle Nothing f)) print
		(waitFor (adjust windowNew >>= adjust . storeDefaultWindow) >> s)
		Nothing <* closeField f
