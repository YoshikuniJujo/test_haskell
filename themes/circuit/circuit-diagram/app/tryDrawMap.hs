{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram.Map (
	DiagramMapM, execDiagramMapM, ElementId, Element(..),
	connectLine, newElementWithPos,
	inputPosition, inputPosition2, newElement0 )
import Circuit.Diagram.Draw (drawDiagram)

main :: IO ()
main = case sample2 `execDiagramMapM` 2 of
	Right s2 -> renderSVG "sample5.svg" (mkWidth 600) $ drawDiagram s2
	Left emsg -> putStrLn $ "no diagram: " ++ emsg

eid0, eid1, eid2, eid3, eid4 :: ElementId
[eid0, eid1, eid2, eid3, eid4] = ["0", "1", "2", "3", "4"]

sample2 :: DiagramMapM ()
sample2 = do
	ip0 <- inputPosition =<< newElement0 eid0 NotGateE
	ip1 <- inputPosition2 =<< newElementWithPos eid1 AndGateE ip0
	() <$ newElementWithPos eid2 OrGateE ip0
	() <$ newElementWithPos eid3 AndGateE ip1
	connectLine eid0 0 eid2
	connectLine eid2 0 eid3
	connectLine eid2 1 eid2
	connectLine eid1 1 eid3
	() <$ newElementWithPos eid4 NotGateE ip0
