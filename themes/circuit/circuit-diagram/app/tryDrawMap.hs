{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude as P

import Data.Map
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG

import Circuit.Diagram.Map
import Circuit.Diagram.Draw

main :: IO ()
main = do
	renderSVG "sample4.svg" (mkWidth 600) $ drawDiagram sample1
	maybe (return ())
		(renderSVG "sample5.svg" (mkWidth 600) . drawDiagram) sample2

sample1 :: DiagramMap
sample1 = DiagramMap {
	width = 5,
	height = 2,
	layout = P.foldr (uncurry insert) empty  [
		((0, 0), HLine),
		((1, 0), NotGateE),
		((3, 0), HLine),
		((4, 0), AndGateE) ] }

sample2 :: Maybe DiagramMap
sample2 = generateDiagramMap 7 4 $ do
	putElement 1 NotGateE
	nextLevel
	putElement 1 AndGateE
	putElement 1 OrGateE
