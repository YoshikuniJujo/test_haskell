{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram (
	drawDiagram, DiagramMapM, execDiagramMapM, ElementId,
	andGateD, orGateD, notGateD,
	connectLine, connectLine1, connectLine2, newElement,
	inputPosition, inputPosition2, newElement0 )

main :: IO ()
main = case sample2 `execDiagramMapM` 2 of
	Right s2 -> renderSVG "sample5.svg" (mkWidth 600) $ drawDiagram s2
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4 :: ElementId
[eid0, eid1, eid2, eid3, eid4] = ["0", "1", "2", "3", "4"]

sample2 :: DiagramMapM ()
sample2 = do
	ip0 <- inputPosition =<< newElement0 eid0 notGateD
	ip1 <- inputPosition2 =<< newElement eid1 andGateD ip0
	() <$ newElement eid2 orGateD ip0
	() <$ newElement eid3 andGateD ip1
	connectLine eid0 eid2
	connectLine1 eid2 eid3
	connectLine2 eid2 eid2
	connectLine2 eid1 eid3
	() <$ newElement eid4 notGateD ip0
