{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Time

import MonadicFrp
import Field

import BoxHandler
import ColorToPixel

main :: IO ()
main = do
	f <- openField "複数の箱" [exposureMask, buttonPressMask, buttonReleaseMask, pointerMotionMask]
	ct <- getCurrentTime
	_ <- (`runStateT` ct) $ interpretSig (handle f) (liftIO . fillBoxes f) boxes
	closeField f

fillBoxes :: Field -> [Box] -> IO ()
fillBoxes f bs = do
	clearField f
	fillBox f `mapM_` reverse bs
	flushField f

fillBox :: Field -> Box -> IO ()
fillBox f (Box (Rect (l, t) (r, u)) c) =
	fillRect f (color c)
		(round $ min l r) (round $ min t u)
		(round . abs $ r - l) (round . abs $ u - t)
