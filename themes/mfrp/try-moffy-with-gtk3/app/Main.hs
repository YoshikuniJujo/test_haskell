{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Moffy (adjust, waitFor, break)
import Control.Moffy.Event.Delete (deleteEvent)
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow

import Trial.Boxes (boxes)
import Trial.Boxes.RunXField (runBoxes)

main :: IO ()
main = () <$ runBoxes "TRY BOXES" do
	i <- waitFor $ adjust windowNew
	waitFor . adjust $ storeDefaultWindow i
	boxes `break` deleteEvent i
