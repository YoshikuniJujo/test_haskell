{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Prelude as P

import Data.Map
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG

import Circuit.Diagram.Map
import Circuit.Diagram.Draw
import AStar.AStar

main :: IO ()
main = do
	renderSVG "sample4.svg" (mkWidth 600) $ drawDiagram sample1
	case sample2 of
		Just ((p0, p1), s2) -> do
			renderSVG "sample5.svg" (mkWidth 600) $ drawDiagram s2
			print $ astar DiagramMapAStar {
				startLine = p0, endLine = p1, diagramMapA = s2 }
		Nothing -> return ()

sample1 :: DiagramMap
sample1 = DiagramMap {
	width = 5,
	height = 2,
	layout = P.foldr (uncurry insert) empty  [
		((Pos 0 0), HLine),
		((Pos 1 0), NotGateE),
		((Pos 3 0), HLine),
		((Pos 4 0), AndGateE) ] }

sample2 :: Maybe ((Pos, [Pos]), DiagramMap)
sample2 = runDiagramMapM 15 15 $ do
	_ <- putElement0 (ElementId 0) NotGateE 2
	_ <- putElement (ElementId 1) AndGateE 7
	_ <- putElement (ElementId 2) OrGateE 7
	_ <- putElement (ElementId 3) AndGateE 14
	lp0 <- getElementPos $ ElementId 0
	lp2 <- getElementPos $ ElementId 2
	let	p0 = head $ inputLinePos lp0
	connectLine p0 $ ElementId 2
	let	p2 = head $ inputLinePos lp2
	connectLine p2 $ ElementId 3
	let	p4 = head . tail $ inputLinePos lp2
		p5 = outputLinePos lp2
	connectLine p4 $ ElementId 2


	lp4 <- getElementPos $ ElementId 1
	let	p6 = head . tail $ inputLinePos lp4
	connectLine p6 $ ElementId 3

	_ <- putElementWithPos (ElementId 4) NotGateE (Pos 7 3)

	return (p4, p5)
