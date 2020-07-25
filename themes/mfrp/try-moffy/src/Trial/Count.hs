{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Count where

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Type.Set
import Data.Or
import System.Random

import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Delete
import Control.Moffy.Handle.XField.Mouse
import Control.Moffy.Handle.XField
import Field

import Data.Time
import Data.OneOrMore

type MouseEv' = DeleteEvent :- MouseEv

handleMouse :: Maybe DiffTime -> Field -> Handle' IO (DeleteEvent :- MouseEv)
handleMouse mprd f rqs = handleWith (\case MouseEv e -> Just $ Data.OneOrMore.expand e; _ -> Nothing) mprd f rqs

leftCount :: Int -> React s MouseEv' Int
leftCount c = adjust (leftClick `first` rightClick) >>= \case
	L () -> leftCount $ c + 1
	R () -> pure c
	LR () () -> pure $ c + 1

tryLeftCount :: IO Int
tryLeftCount = do
	f <- openField "TRY LEFT COUNT" [buttonPressMask, exposureMask]
	interpretReact (retry $ handleMouse Nothing f) (leftCount 0) <* closeField f

leftCountSig :: Int -> Sig s MouseEv' Int Int
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

leftRandomSig :: StdGen -> Sig s MouseEv' Int StdGen
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
