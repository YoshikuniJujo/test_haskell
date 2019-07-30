-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.PictureOrGate (orGateD) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

orGateD :: Diagram B
orGateD = moveTo ((- 3) ^& 0) (orGate ||| line' 0.2)
	`withEnvelope'` (rect 3 3 :: Diagram B)

line2 :: Diagram B
line2 = (strokeT (fromOffsets [zero & _x .~ 0.55]) # lwL 0.08)

line' :: Double -> Diagram B
line' l = (strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08)

orGate, orGate0, orGate1, orGate2, orGate3 :: Diagram B
orGate = orGate0 <> orGate1 <> orGate2 <> moveTo (1.7 ^& 0) orGate3
orGate0 = alignY 0 (vcat' (with & sep .~ 2) [line2, line2])
orGate1 = arcBetween (p2 (0.2, -1.3)) (p2 (0.2, 1.3)) (- 0.7) # lwL 0.08
orGate2 = moveTo (0.169 ^& 0) $ alignY 0 $ vcat' (with & sep .~ 2.6) [line' 1.245, line' 1.245]
orGate3 = (arcBetween (p2 (- 0.3, 1.3)) (p2 (1.1, 0)) 0.2 === arcBetween (p2 (- 0.3, -1.3)) (p2 (1.1, 0)) (- 0.2)) #
	lwL 0.08 -- arc (dir unit_Y) (1 / 2 @@ turn) # lwL 0.08

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope
