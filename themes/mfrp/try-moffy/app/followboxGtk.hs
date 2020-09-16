module Main where

import Trial.Followbox
import Trial.Followbox.RunGtkField

main :: IO ()
main = runFollowbox "firefox" Nothing followbox

