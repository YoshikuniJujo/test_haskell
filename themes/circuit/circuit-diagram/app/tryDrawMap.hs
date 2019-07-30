{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude as P

import Data.Map
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG

import Circuit.Diagram.Map
import Circuit.Diagram.Draw

main :: IO ()
main = renderSVG "sample4.svg" (mkWidth 600) $ drawDiagram sample1

sample1 :: DiagramMap
sample1 = DiagramMap {
	width = 5,
	height = 2,
	layout = P.foldr (uncurry insert) empty  [
		((0, 0), HLine),
		((1, 0), NotGateE),
		((3, 0), HLine),
		((4, 0), AndGateE) ] }
