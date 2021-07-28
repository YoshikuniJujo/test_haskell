{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import System.Environment
import Graphics.Gdk.General

main :: IO ()
main = do
	a : as <- getArgs
	gdkSetAllowedBackends a
	print =<< join (gdkInit <$> getProgName <*> pure as)
	print =<< gdkGetDisplayArgName
	print =<< gdkGetProgramClass
	gdkSetProgramClass "baribari"
	print =<< gdkGetProgramClass
