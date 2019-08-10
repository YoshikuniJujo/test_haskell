{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Draw (drawDiagram) where

import Data.Map.Strict
import Diagrams.Prelude (Diagram, moveTo, (^&), (===))
import Diagrams.Backend.SVG

import Circuit.Diagram.DiagramMap
import Circuit.Diagram.Pictures

drawDiagram :: DiagramMap -> Diagram B
drawDiagram DiagramMap { width = w, height = h, layout = l } = mconcat
	. (<$> [ Pos x y | x <- [0 .. w - 1], y <- [0 .. h - 1] ]) $ \p@(Pos x y) ->
		case l !? p of
			Just e -> moveTo (- fromIntegral x ^& fromIntegral y)
				$ drawElement e
			Nothing -> mempty

drawElement :: ElementDiagram -> Diagram B
drawElement AndGateE = andGateD
drawElement OrGateE = orGateD
drawElement NotGateE = notGateD
drawElement TriGateE = triGateD
drawElement (ConstGateE bs) = constGateD bs
drawElement (DelayE d) = delayD d
drawElement HLine = hlineD
drawElement EndHLine = hlineD
drawElement (HLineText t1 t2) = hlineTextD t1 t2
drawElement VLine = vlineD
drawElement Stump = mempty
drawElement TopLeft = topLeftD
drawElement EndTopLeft = topLeftD
drawElement BottomLeft = bottomLeftD
drawElement TopRight = topRightD
drawElement BottomRight = bottomRightD
drawElement EndBottomLeft = bottomLeftD
drawElement TShape = tshapeD
drawElement TInverted = tishapeD
drawElement TLeft = tlshapeD
drawElement TRight = trshapeD
drawElement Cross = crossD
drawElement CrossDot = crossDotD
drawElement BranchE = tshapeD === topLeftD
drawElement e = error $ "Circuit.Diagram.Draw.drawElement: not yet implemented: " ++ show e
