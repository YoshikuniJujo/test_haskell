{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (repeat)

import Control.Monad.State
import Data.Time

import MonadicFrp
import Field

import BoxHandler

main :: IO ()
main = do
	f <- openField "ダブルクリック" [exposureMask, buttonPressMask, pointerMotionMask]
	ct <- getCurrentTime
	_ <- (`runStateT` ct)
		$ interpretSig (handle f) (liftIO . print) . repeat . drClickOn $ Rect (10, 10) (500, 400)
	closeField f
