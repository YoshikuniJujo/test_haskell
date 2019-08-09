{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.ByteArray (convert)
import Data.ByteString.Char8 (pack)
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram.Map (
	DiagramMapM, execDiagramMapM, ElementIdable(..), Element(..),
	newElement0, newElementWithPos, connectLine,
	inputPosition )
import Circuit.Diagram.Draw (drawDiagram)
import Crypto.Hash (hash, SHA3_256)

main :: IO ()
main = case execDiagramMapM circuitDiagram 3 of
	Right cd -> renderSVG "simple.svg" (mkWidth 600) $ drawDiagram cd
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

data Elem = NotGate Word | Caption Word | Branch Word deriving Show

instance ElementIdable Elem where
	elementIdGen e = convert . hash @_ @SHA3_256 . pack $ pfx ++ show n
		where (pfx, n) = case e of
			NotGate n' -> ("NotGate", n')
			Caption n' -> ("Caption", n')
			Branch n' -> ("Branch", n')

circuitDiagram :: DiagramMapM ()
circuitDiagram = do
	ip0 <- inputPosition =<< newElement0 (NotGate 0) NotGateE
	ip1 <- inputPosition
		=<< newElementWithPos (Caption 0) (HLineText "31:16" "63:32") ip0
	_ <- newElementWithPos (NotGate 1) NotGateE ip1
	connectLine (NotGate 0) 0 (Caption 0)
	connectLine (Caption 0) 0 (NotGate 1)
	connectLine (NotGate 1) 0 (NotGate 1)

	ip2 <- inputPosition =<< newElement0 (NotGate 2) NotGateE
	_ <- newElementWithPos (Branch 0) BranchE ip2
	connectLine (NotGate 2) 0 (NotGate 2)
	connectLine (Branch 0) 0 (NotGate 2)
