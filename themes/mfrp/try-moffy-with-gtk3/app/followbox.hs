{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow

import Trial.Followbox (followbox)
import Trial.Followbox.RunXField (evalFollowbox)

main :: IO ()
main = evalFollowbox "FOLLOW BOX" do
	i <- waitFor $ adjust windowNew
	waitFor . adjust $ storeDefaultWindow i
	adjustSig $ followbox i
