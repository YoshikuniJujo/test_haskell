module Main where

import Control.Moffy
import Control.Moffy.Event.Window

import Trial.Followbox
import Trial.Followbox.RunGtkField

main :: IO ()
main = runFollowbox "firefox" Nothing (waitFor (adjust windowNew) >> followbox)
