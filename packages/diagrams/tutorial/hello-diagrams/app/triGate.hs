-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
-- example = line ||| notGate ||| line
example = notGate ||| lineRight 0.3

line :: Diagram B
line = strokeT (fromOffsets [unitX]) # lwL 0.08

lineUp :: Double -> Diagram B
lineUp l = strokeT (fromOffsets [zero &_y .~ l]) # lwL 0.08

lineRight :: Double -> Diagram B
lineRight l = strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08

notGate, notGate0, notGate1, notGate2 :: Diagram B
notGate = notGate0 <> notGate1 <> notGate2 <> notGate3
notGate0 = (triangle1_4 1 `withEnvelope'` triangle1_4 1.03) # lwL 0.08
notGate1 =  moveTo (0 ^& 0.33) $ lineUp 0.5
notGate2 = moveTo (0 ^& 0.83) $ lineRight (- 0.8)
notGate3 = moveTo ((- 0.3) ^& 0) $ lineRight (- 0.5)

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope

triangle1_4 :: Double -> Diagram B
triangle1_4 = rotateBy (- 1 / 4) . triangle

circleB :: Double -> Diagram B
circleB = circle

main :: IO ()
main = mainWith example
