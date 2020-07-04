{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Count where

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Or
import System.Random

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

leftCountSig :: Int -> Sig s MouseEv Int Int
leftCountSig c = do
	emit c
	waitFor (adjust $ leftClick `first` rightClick) >>= \case
		L () -> leftCountSig $ c + 1
		R () -> pure c
		LR () () -> pure $ c + 1

tryLeftCountSig :: IO Int
tryLeftCountSig = do
	f <- openField "TRY LEFT COUNT SIG" [buttonPressMask, exposureMask]
	interpret (retry $ handleMouse Nothing f) print (leftCountSig 0) <* closeField f

leftRandomSig :: StdGen -> Sig s MouseEv Int StdGen
leftRandomSig g = do
	let	(i, g') = random g
	emit i
	waitFor (adjust $ leftClick `first` rightClick) >>= \case
		L () -> leftRandomSig g'
		R () -> pure g'
		LR () () -> pure g'

tryLeftRandomSig :: IO StdGen
tryLeftRandomSig = do
	f <- openField "TRY LEFT RANDOM SIG" [buttonPressMask, exposureMask]
	interpret (retry $ handleMouse Nothing f) print (leftRandomSig $ mkStdGen 8) <* closeField f

-- tryTry :: React s MouseEv (Int, Int -> (React s MouseEv ...
