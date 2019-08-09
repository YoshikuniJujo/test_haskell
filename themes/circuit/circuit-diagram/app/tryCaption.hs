{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram.Map (
	DiagramMapM, execDiagramMapM, ElementId,
	andGateE, notGateE, branchE, hLineText,
	newElement0, newElement,
	inputPosition, inputPosition1, inputPosition2, connectLine )
import Circuit.Diagram.Draw (drawDiagram)

main :: IO ()
main = case sample2 `execDiagramMapM` 2 of
	Right s2 -> renderSVG "caption.svg" (mkWidth 600) $ drawDiagram s2
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

eid0, eid1, eid2, eid5, eid6, eid100, eid101, eid102, eid103, eid104 :: ElementId
[eid0, eid1, eid2, eid5, eid6, eid100, eid101, eid102, eid103, eid104] =
	["0", "1", "2", "5", "6", "100", "101", "102", "103", "104"]


sample2 :: DiagramMapM ()
sample2 = do
	ip0 <- inputPosition =<< newElement0 eid0 notGateE
	ip1 <- inputPosition =<< newElement eid100 (hLineText "63:32" "31:0") ip0
	ip2 <- inputPosition2 =<< newElement eid1 andGateE ip1
	il3 <- newElement eid102 branchE ip2
	ip3 <- inputPosition1 il3
	ip4 <- inputPosition2 il3
	connectLine eid0 0 eid100
	connectLine eid100 0 eid1
	connectLine eid1 1 eid102
	ip5 <- inputPosition =<< newElement eid103 (hLineText "62:0" "63:1") ip3
	ip6 <- inputPosition =<< newElement eid104 (hLineText "0:0" "0:0") ip4
	connectLine eid102 0 eid103
	connectLine eid102 1 eid104
	() <$ newElement eid5 notGateE ip5
	() <$  newElement eid6 notGateE ip6
	connectLine eid103 0 eid5
	connectLine eid104 0 eid6

	ip7 <- inputPosition =<< newElement0 eid2 notGateE
	() <$ newElement eid101 branchE ip7
	connectLine eid2 0 eid2
	connectLine eid101 0 eid2
