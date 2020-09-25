{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy
import Control.Moffy.Event.Window

import Trial.Followbox
import Trial.Followbox.RunGtkField

main :: IO ()
main = runFollowbox "firefox" Nothing do
	i <- waitFor $ adjust windowNew
	followbox i
