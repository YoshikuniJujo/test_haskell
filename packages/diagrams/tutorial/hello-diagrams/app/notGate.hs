-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = line ||| notGate ||| line

line :: Diagram B
line = strokeT (fromOffsets [unitX]) # lwL 0.08

notGate :: Diagram B
notGate = (triangle1_4 2 `withEnvelope'` triangle1_4 2.03 ||| circleB (2 / 8) `withEnvelope'` circleB (2 / 8 + 0.03)) # lwL 0.08

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope

triangle1_4 :: Double -> Diagram B
triangle1_4 = rotateBy (- 1 / 4) . triangle

circleB :: Double -> Diagram B
circleB = circle

main :: IO ()
main = mainWith example
