{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (repeat)

import Control.Monad.State
import Data.Time

import MonadicFrp
import Field

import BoxHandler
import ColorToPixel

main :: IO ()
main = do
	f <- openField "箱" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	ct <- getCurrentTime
	_ <- (`runStateT` ct) $ interpretSig (handle f) (liftIO . fillBox f) box
	closeField f

fillBox :: Field -> Box -> IO ()
fillBox f (Box (Rect (l, t) (r, u)) c) = do
	clearField f
	fillRect f (color c) (round l) (round t) (round $ r - l) (round $ u - t)
	flushField f
