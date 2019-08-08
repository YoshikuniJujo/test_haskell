{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG

import Circuit.Diagram.Map
import Circuit.Diagram.Draw

main :: IO ()
main = case sample2 of
		Right s2 -> renderSVG "simple.svg" (mkWidth 600) $ drawDiagram s2
		Left emsg -> putStrLn $ "Can't draw diagram " ++ emsg

sample2 :: Either String DiagramMap
sample2 = execDiagramMapM 15 8 $ do
	_ <- putElement0 (ElementId 0) NotGateE 2
	_ <- putElementWithPos (ElementId 100) (HLineText "31:16" "63:32") (Pos 7 0)
	_ <- putElement (ElementId 1) NotGateE 11
	lp0 <- getElementPos $ ElementId 0
	lp1 <- getElementPos $ ElementId 1
	lp100 <- getElementPos $ ElementId 100
	let	p0 = head $ inputLinePos lp0
		p1 = head $ inputLinePos lp1
		p100 = head $ inputLinePos lp100
	connectLine p0 $ ElementId 100
	connectLine p100 $ ElementId 1
	connectLine p1 $ ElementId 1
	_ <- putElement0 (ElementId 2) NotGateE 2
	lp2 <- getElementPos $ ElementId 2
	let	p2 = head $ inputLinePos lp2
	connectLine p2 $ ElementId 2

	_ <- putElementWithPos (ElementId 101) Branch (Pos 7 6)
	p101 <- head . inputLinePos <$> getElementPos (ElementId 101)
	connectLine p101 $ ElementId 2
	return ()
