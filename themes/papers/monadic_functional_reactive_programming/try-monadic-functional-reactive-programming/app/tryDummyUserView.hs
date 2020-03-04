{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.String
import System.Environment

import Signal
import Field

import Followbox
import Followbox.DummyHandle
import Followbox.Viewer

main :: IO ()
main = do
	args <- getArgs
	let	nmtkn = case args of
			[] -> Nothing
			[nm, tkn] -> Just (fromString nm, tkn)
			_ -> error "bad arguments"
	f <- openField ("GitHubのユーザを表示するよ" :: String) [exposureMask, buttonPressMask]
	interpretSig (handle' f nmtkn) (liftIO . debugView f) usersView' `runStateT` ([], [], Nothing) >>= print
	closeField f

debugView :: (Show n, Integral n) => Field -> View n -> IO ()
debugView f v = do
	printText `mapM_` v
	view f v
