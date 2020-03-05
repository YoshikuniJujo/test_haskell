{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State

import Signal
import Field

import Followbox
import Followbox.DummyHandle
import Followbox.Viewer

main :: IO ()
main = do
	f <- openField ("GitHubのユーザを表示するよ" :: String) [exposureMask, buttonPressMask]
	interpretSig (handle' f) (liftIO . debugView f) usersView' `runStateT` ([], [], Nothing) >>= print
	closeField f

debugView :: (Show n, Integral n) => Field -> View n -> IO ()
debugView f v = do
	printText `mapM_` v
	view f v
