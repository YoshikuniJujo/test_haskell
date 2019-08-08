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
	case sample2 of
		Right s2 ->
			renderSVG "caption.svg" (mkWidth 600) $ drawDiagram s2
		Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

sample1 :: DiagramMap
sample1 = DiagramMap {
	width = 5,
	height = 2,
	layout = P.foldr (uncurry insert) empty  [
		((Pos 0 0), HLine),
		((Pos 1 0), NotGateE),
		((Pos 3 0), HLine),
		((Pos 4 0), AndGateE) ] }

sample2 :: Either String DiagramMap
sample2 = execDiagramMapM 16 8 $ do
	_ <- putElement0 (ElementId 0) NotGateE 2
	_ <- putElementWithPos (ElementId 100) (HLineText "63:32" "31:0") (Pos 5 1)
	_ <- putElement (ElementId 1) AndGateE 7
	_ <- putElementWithPos (ElementId 102) Branch (Pos 11 2)
	lp0 <- getElementPos $ ElementId 0
	lp1 <- getElementPos $ ElementId 1
	lp100 <- getElementPos $ ElementId 100
	p102 <- head . inputLinePos <$> getElementPos (ElementId 102)
	p102_2 <- head . tail . inputLinePos <$> getElementPos (ElementId 102)
	let	p0 = head $ inputLinePos lp0
		p12 = head . tail $ inputLinePos lp1
		p100 = head $ inputLinePos lp100
	connectLine p12 $ ElementId 102
	connectLine p0 $ ElementId 100
	connectLine p100 $ ElementId 1
	_ <- putElementWithPos (ElementId 103) (HLineText "62:0" "63:1") (Pos 13 2)
	_ <- putElementWithPos (ElementId 104) (HLineText "0:0" "0:0") (Pos 13 3)
	p103 <- head . inputLinePos <$> getElementPos (ElementId 103)
	p104 <- head . inputLinePos <$> getElementPos (ElementId 104)
	connectLine p102 $ ElementId 103
	connectLine p102_2 $ ElementId 104
	_ <- putElement (ElementId 5) NotGateE 15
	_ <- putElement (ElementId 6) NotGateE 15
	connectLine p103 $ ElementId 5
	connectLine p104 $ ElementId 6

	_ <- putElement0 (ElementId 2) NotGateE 2
	lp2 <- getElementPos $ ElementId 2
	let	p2 = head $ inputLinePos lp2
	connectLine p2 $ ElementId 2

	_ <- putElementWithPos (ElementId 101) Branch (Pos 7 6)
	p101 <- head . inputLinePos <$> getElementPos (ElementId 101)
	connectLine p101 $ ElementId 2
	return ()
