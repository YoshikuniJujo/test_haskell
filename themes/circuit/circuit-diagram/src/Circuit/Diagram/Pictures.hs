{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Pictures (
	andGateD, orGateD, notGateD, triGateD, constGateD, delayD,
	hlineD, vlineD, topLeftD, bottomLeftD, topRightD, bottomRightD,
	tshapeD, tishapeD, tlshapeD, trshapeD, crossD, crossDotD,
	hlineTextD ) where

import Data.Word
import Numeric
import Diagrams.Prelude
import Diagrams.Direction
import Diagrams.Backend.SVG

import Circuit.Diagram.PictureOrGate

notGateD :: Diagram B
notGateD = moveTo ((- 1) ^& 0)
	$ (moveTo ((- 1) ^& 0) (lineRight 0.1) <> notGateDPure <> moveTo (1 ^& 0) (lineRight (- 0.15)))
		`withEnvelope'` (rect 2 3 :: Diagram B)

notGateDPure :: Diagram B
notGateDPure = (moveTo ((- 0.45) ^& 0) (triangle1_4 1.5) <> moveTo (0.66 ^& 0) (circle (1.5 / 8))) # lwL 0.08

triGateD :: Diagram B
triGateD =
	moveTo ((- 2) ^& 2) (lineRight 0.74) <>
	moveTo ((- 1.3) ^& 2) (lineBottom (- 1.6)) <>
	moveTo ((- 1) ^& 0)
		((moveTo ((- 1) ^& 0) (lineRight 0.1) <> triGateDPure <> moveTo (1 ^& 0) (lineRight (- 0.55))))

triGateDPure :: Diagram B
triGateDPure = moveTo ((- 0.45) ^& 0) (triangle1_4 1.5) # lwL 0.08

triangle1_4 :: Double -> Diagram B
triangle1_4 = rotateBy (- 1 / 4) . triangle

constGateD :: Word64 -> Diagram B
constGateD bs = moveTo ((- 1.5) ^& 0)
	$ rect 3 0.8 # lwL 0.08 <> text (showHex bs "") # scale 0.340

delayD, delayDPure :: Word8 -> Diagram B
delayD d =
	moveTo ((- 2) ^& 0) (lineRight 0.5) <> delayDPure d <> moveTo (0 ^& 0) (lineRight (- 0.5))

delayDPure d = moveTo ((- 1) ^& 0)
	$ rect 1 0.85 # lwL 0.08 <> text (show d) # scale 0.6

andGateD :: Diagram B
andGateD = moveTo ((- 1.5) ^& 0) $ (andGateIs <> andGatePure <> moveTo (1.5 ^& 0) (lineRight (- 0.2)))
	`withEnvelope'` (rect 3 3 :: Diagram B)

andGatePure, andGate1, andGate2 :: Diagram B
andGatePure = andGate1 <> andGate2
andGate1 = fromVertices (map p2 [(0, 1.3), (- 1.3, 1.3), (- 1.3, - 1.3), (0, - 1.3)]) # lwL 0.08
andGate2 = moveTo (0 ^& 0) $ scale 1.3 (arc (dir unit_Y) (1 / 2 @@ turn)) # lwL 0.08

andGateIs, andGateI1, andGateI2 :: Diagram B
andGateIs = andGateI1 <> andGateI2
andGateI1 = moveTo ((- 1.5) ^& 1) $ lineRight 0.2
andGateI2 = moveTo ((- 1.5) ^& (- 1)) $ lineRight 0.2

withEnvelope' :: (InSpace v n a, Monoid' m, Enveloped a) =>
	QDiagram b v n m -> a -> QDiagram b v n m
withEnvelope' = flip withEnvelope

lineRight, lineBottom :: Double -> Diagram B
lineRight l = strokeT (fromOffsets [zero &_x .~ l]) # lwL 0.08
lineBottom l = strokeT (fromOffsets [zero &_y .~ l]) # lwL 0.08

hlineD, vlineD :: Diagram B
hlineD = moveTo ((- 1) ^& 0) (strokeT (fromOffsets [unitX]) # lwL 0.08)
	`withEnvelope'` (rect 1 1 :: Diagram B)

vlineD = moveTo ((- 0.5) ^& (- 0.5)) $ strokeT (fromOffsets [unitY]) # lwL 0.08

topLeftD, bottomLeftD, topRightD, bottomRightD :: Diagram B
topLeftD = reflectY bottomLeftD
topRightD =
	(strokeT (fromOffsets [zero &_x .~ (- 0.5), zero &_y .~ 0.5]) # lwL 0.08)
		`withEnvelope'` (rect 2 1 :: Diagram B)
bottomLeftD = moveTo ((- 1) ^& 0) $ rotateBy (1 / 2) topRightD
bottomRightD = reflectY topRightD

tshapeD :: Diagram B
tshapeD = hlineD <> vlineH <> dotD

vlineH :: Diagram B
vlineH = moveTo ((- 0.5) ^& (- 0.5)) $ lineBottom 0.5

tishapeD :: Diagram B
tishapeD = hlineD <> vlineH' <> dotD

vlineH' :: Diagram B
vlineH' = moveTo ((- 0.5) ^& 0) $ lineBottom 0.5

tlshapeD, trshapeD :: Diagram B
tlshapeD = hlineH <> vlineD <> dotD
trshapeD = hlineHR <> vlineD <> dotD

hlineH, hlineHR :: Diagram B
hlineH = moveTo ((- 0.5) ^& 0) $ lineRight 0.5
hlineHR = moveTo ((- 1) ^& 0) $ lineRight 0.5

dotD :: Diagram B
dotD = moveTo ((- 0.5) ^& 0) $ circle (1.0 / 8) # fc black

crossD, crossDotD :: Diagram B
crossD = hlineD <> vlineD
crossDotD  = crossD <> dotD

hlineTextD :: String -> String -> Diagram B
hlineTextD t1 t2 =
	moveTo ((- 0.9) ^& 0.2) (text t1 # scale (1 / 4)) <>
	moveTo ((- 0.5) ^& 0.2) (text "-" # scale (1 / 4)) <>
	moveTo ((- 0.1) ^& 0.2) (text t2 # scale (1 / 4)) <>
	hlineD
