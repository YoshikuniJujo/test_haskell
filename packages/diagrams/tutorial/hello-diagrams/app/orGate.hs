-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Diagrams.Prelude
import Diagrams.Direction
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = alignY 0 (vcat' (with & sep .~ 1) [line, line]) {- # showOrigin -} ||| orGate ||| line

orGateD :: Diagram B
orGateD = moveTo ((- 1.5) ^& 0) (orGate ||| line' 0.2)
	`withEnvelope'` (rect 3 3 :: Diagram B)

line :: Diagram B
line = (strokeT (fromOffsets [unitX]) # lwL 0.08)

line2 :: Diagram B
line2 = (strokeT (fromOffsets [zero & _x .~ 0.3]) # lwL 0.08)

line' :: Double -> Diagram B
line' l = (strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08)

orGate, orGate0, orGate1, orGate2, orGate3 :: Diagram B
orGate = orGate0 <> orGate1 <> orGate2 <> moveTo (1.7 ^& 0) orGate3
orGate0 = alignY 0 (vcat' (with & sep .~ 2) [line2, line2])
orGate1 = arcBetween (p2 (0.2, -1.3)) (p2 (0.2, 1.3)) (- 0.3) # lwL 0.08
orGate2 = moveTo (0.169 ^& 0) $ alignY 0 $ vcat' (with & sep .~ 2.6) [line' 1.545, line' 1.545]
orGate3 = (arcBetween (p2 (0, 1.3)) (p2 (1.1, 0)) 0.2 === arcBetween (p2 (0, -1.3)) (p2 (1.1, 0)) (- 0.2)) #
	lwL 0.08 -- arc (dir unit_Y) (1 / 2 @@ turn) # lwL 0.08

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope

triangle1_4 :: Double -> Diagram B
triangle1_4 = rotateBy (- 1 / 4) . triangle

circleB :: Double -> Diagram B
circleB = circle

squareB :: Double -> Diagram B
squareB = square

main :: IO ()
main = mainWith orGateD
