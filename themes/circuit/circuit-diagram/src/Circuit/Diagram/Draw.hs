{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Draw where

import Diagrams.Prelude
import Diagrams.Backend.SVG

notGateD :: Diagram B
notGateD = moveTo ((- 1) ^& 0)
	$ (moveTo ((- 1) ^& 0) (lineRight 0.1) <> notGateDPure <> moveTo (1 ^& 0) (lineRight (- 0.15)))
		`withEnvelope'` (rect 2 3 :: Diagram B)

notGateDPure :: Diagram B
notGateDPure = (moveTo ((- 0.45) ^& 0) (triangle1_4 1.5) <> moveTo (0.66 ^& 0) (circle (1.5 / 8))) # lwL 0.08

triangle1_4 :: Double -> Diagram B
triangle1_4 = rotateBy (- 1 / 4) . triangle

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope

lineRight :: Double -> Diagram B
lineRight l = strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08
