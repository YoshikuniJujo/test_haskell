module Main where

import Trials.Boxes
import Trials.Boxes.Run

main :: IO ()
main = withInterpretSig "Boxes" (\f -> withFlush f . (drawBox f `mapM_`) . reverse) boxes
