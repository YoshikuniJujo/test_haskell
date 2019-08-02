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
				startLine = p0 { posX = posX p0 }, endLine = p1 { posX = posX p1 },
				diagramMapA = s2 }
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

sample2 :: Maybe ((Pos, Pos), DiagramMap)
sample2 = runDiagramMapM 15 8 $ do
	_ <- putElement0 (ElementId 0) NotGateE 2
	_ <- putElement (ElementId 1) AndGateE 7
	_ <- putElement (ElementId 2) OrGateE 7
	_ <- putElement (ElementId 3) AndGateE 14
	lp0 <- getElementPos $ ElementId 0
	lp2 <- getElementPos $ ElementId 2
	let	p0 = head $ inputLinePos lp0
		p1 = outputLinePos lp2
	connectLine p0 p1
	lp3 <- getElementPos $ ElementId 3
	let	p2 = head $ inputLinePos lp2
		p3 = outputLinePos lp3
	connectLine p2 p3
	let	p4 = head . tail $ inputLinePos lp2
		p5 = outputLinePos lp2
	connectLine p4 p5
	return (p4, p5)
