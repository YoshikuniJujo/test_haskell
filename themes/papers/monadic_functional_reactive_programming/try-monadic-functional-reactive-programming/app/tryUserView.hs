{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State

import Signal
import Field

import Followbox
import FollowboxHandle

import qualified FollowboxView as V

view :: Integral n => Field -> V.View n -> FollowboxIO ()
view f = liftIO . V.view f

main :: IO ()
main = do
	f <- openField ("GitHubのユーザを表示するよ" :: String) [exposureMask, buttonPressMask]
	interpretSig (handle f) (view f) usersView `runStateT` ([], []) >>= print
	closeField f
