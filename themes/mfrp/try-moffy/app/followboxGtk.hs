module Main where

import Trial.Followbox
import Trial.Followbox.RunGtk

main :: IO ()
main = runFollowbox "firefox" Nothing followbox

