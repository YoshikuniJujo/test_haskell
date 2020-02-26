{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State

import Signal
import Field

import Followbox
import FollowboxHandleView

main :: IO ()
main = do
	f <- openField ("GitHubのユーザを表示するよ" :: String) [exposureMask, buttonPressMask]
	interpretSig (handle f) (either error (view f)) userView `runStateT` ([], []) >>= print
	closeField f
