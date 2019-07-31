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
		((Pos 0 0), HLine),
		((Pos 1 0), NotGateE),
		((Pos 3 0), HLine),
		((Pos 4 0), AndGateE) ] }

sample2 :: Maybe DiagramMap
sample2 = generateDiagramMap 9 6 $ do
	nextLevel NotGateE
	_ <- putElement0 NotGateE
	nextLevel NotGateE
	_ <- putElement AndGateE
	putElement OrGateE
