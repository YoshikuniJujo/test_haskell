{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram (
	drawDiagram, DiagramMapM, execDiagramMapM, ElementId,
	andGateD, notGateD, branchD, hLineTextD,
	newElement0, newElement,
	inputPosition, inputPosition1, inputPosition2,
	connectLine, connectLine1, connectLine2 )

main :: IO ()
main = case sample2 `execDiagramMapM` 2 of
	Right s2 -> renderSVG "caption.svg" (mkWidth 600) $ drawDiagram s2
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

eid0, eid1, eid2, eid5, eid6, eid100, eid101, eid102, eid103, eid104 :: ElementId
[eid0, eid1, eid2, eid5, eid6, eid100, eid101, eid102, eid103, eid104] =
	["0", "1", "2", "5", "6", "100", "101", "102", "103", "104"]


sample2 :: DiagramMapM ()
sample2 = do
	ip0 <- inputPosition =<< newElement0 eid0 notGateD
	ip1 <- inputPosition =<< newElement eid100 (hLineTextD "63:32" "31:0") ip0
	ip2 <- inputPosition2 =<< newElement eid1 andGateD ip1
	il3 <- newElement eid102 branchD ip2
	ip3 <- inputPosition1 il3
	ip4 <- inputPosition2 il3
	connectLine eid0 eid100
	connectLine eid100 eid1
	connectLine2 eid1 eid102
	ip5 <- inputPosition =<< newElement eid103 (hLineTextD "62:0" "63:1") ip3
	ip6 <- inputPosition =<< newElement eid104 (hLineTextD "0:0" "0:0") ip4
	connectLine1 eid102 eid103
	connectLine2 eid102 eid104
	() <$ newElement eid5 notGateD ip5
	() <$  newElement eid6 notGateD ip6
	connectLine eid103 eid5
	connectLine eid104 eid6

	ip7 <- inputPosition =<< newElement0 eid2 notGateD
	() <$ newElement eid101 branchD ip7
	connectLine eid2 eid2
	connectLine1 eid101 eid2
