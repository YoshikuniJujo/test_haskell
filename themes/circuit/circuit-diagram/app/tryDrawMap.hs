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
				startLine = p0 { posX = posX p0 + 1 }, endLine = p1 { posX = posX p1 - 1 },
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
sample2 = runDiagramMapM 9 6 $ do
	nextLevel NotGateE
	lp0 <- putElement0 NotGateE
	nextLevel NotGateE
	_ <- putElement AndGateE
	lp2 <- putElement OrGateE
	return (head $ inputLinePos lp0, outputLinePos lp2)
