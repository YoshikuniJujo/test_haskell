-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Diagrams.Prelude
import Diagrams.Direction
import Diagrams.Backend.SVG.CmdLine

example :: Diagram B
example = alignY 0 (vcat' (with & sep .~ 1) [line, line]) {- # showOrigin -} |||
	andGate ||| line

line :: Diagram B
line = (strokeT (fromOffsets [unitX]) # lwL 0.08)

andGate, andGate1, andGate2 :: Diagram B
andGate = andGate1 ||| andGate2
andGate1 = fromVertices (map p2 [(0.8, 1), (-0.8, 1), (-0.8, -1), (0.8, -1)]) # lwL 0.08
andGate2 = arc (dir unit_Y) (1 / 2 @@ turn) # lwL 0.08

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
main = mainWith example
