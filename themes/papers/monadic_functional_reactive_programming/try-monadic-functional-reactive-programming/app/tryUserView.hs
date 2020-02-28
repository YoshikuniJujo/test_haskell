{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State

import Signal
import Field

import Followbox
import FollowboxHandle
import FollowboxViewer

main :: IO ()
main = do
	f <- openField ("GitHubのユーザを表示するよ" :: String) [exposureMask, buttonPressMask]
	interpretSig (handle f) (liftIO . view f) usersView `runStateT` ([], []) >>= print
	closeField f
