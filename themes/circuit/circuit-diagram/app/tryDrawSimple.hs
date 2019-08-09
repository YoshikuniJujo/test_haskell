{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.ByteArray (convert)
import Data.ByteString.Char8 (pack)
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram.Map (
	DiagramMapM, execDiagramMapM, ElementIdable(..),
	notGateE, triGateE, branchE, hLineText,
	newElement0, newElement, connectLine, connectLine1, connectLine2,
	inputPosition, inputPosition1, inputPosition2 )
import Circuit.Diagram.Draw (drawDiagram)
import Crypto.Hash (hash, SHA3_256)

main :: IO ()
main = case execDiagramMapM circuitDiagram 3 of
	Right cd -> renderSVG "simple.svg" (mkWidth 600) $ drawDiagram cd
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

data Elem = NotGate Word | TriGate Word | Caption Word | Branch Word
	deriving Show

instance ElementIdable Elem where
	elementIdGen e = convert . hash @_ @SHA3_256 . pack $ pfx ++ show n
		where (pfx, n) = case e of
			NotGate n' -> ("NotGate", n')
			TriGate n' -> ("TriGate", n')
			Caption n' -> ("Caption", n')
			Branch n' -> ("Branch", n')

circuitDiagram :: DiagramMapM ()
circuitDiagram = do
	ip0 <- inputPosition =<< newElement0 (NotGate 0) notGateE
	ip1 <- inputPosition
		=<< newElement (Caption 0) (hLineText "31:16" "63:32") ip0
	_ <- newElement (NotGate 1) notGateE ip1
	connectLine (NotGate 0) (Caption 0)
	connectLine (Caption 0) (NotGate 1)
	connectLine (NotGate 1) (NotGate 1)

	ip2 <- inputPosition =<< newElement0 (NotGate 2) notGateE
	lp3 <- newElement (Branch 0) branchE ip2
	connectLine (NotGate 2) (NotGate 2)
	connectLine1 (Branch 0) (NotGate 2)

	ip3 <- inputPosition2 lp3
	lp4 <- newElement (TriGate 0) triGateE ip3
	connectLine2 (Branch 0) (TriGate 0)
	ip4 <- inputPosition1 lp4
	ip5 <- inputPosition2 lp4
	() <$ newElement (NotGate 3) notGateE ip4
	() <$ newElement (NotGate 4) notGateE ip5
	connectLine1 (TriGate 0) (NotGate 3)
	connectLine2 (TriGate 0) (NotGate 4)
