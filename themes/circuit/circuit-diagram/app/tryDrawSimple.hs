{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.ByteArray (convert)
import Data.ByteString.Char8 (pack)
import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram (
	drawDiagram, DiagramMapM, execDiagramMapM, ElementIdable(..),
	notGateD, triGateD, constGateD, delayD, hLineD, branchD, hLineTextD,
	newElement0, newElement, connectLine, connectLine1, connectLine2,
	inputPosition, inputPosition1, inputPosition2 )
import Crypto.Hash (hash, SHA3_256)

main :: IO ()
main = case execDiagramMapM circuitDiagram 3 of
	Right cd -> renderSVG "simple.svg" (mkWidth 800) $ drawDiagram cd
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

data Elem
	= NotGate Word | TriGate Word | ConstGate Word | Delay Word
	| Caption Word | Branch Word | IdGate Word
	deriving Show

instance ElementIdable Elem where
	elementIdGen e = convert . hash @_ @SHA3_256 . pack $ pfx ++ show n
		where (pfx, n) = case e of
			NotGate n' -> ("NotGate", n')
			TriGate n' -> ("TriGate", n')
			ConstGate n' -> ("ConstGAte", n')
			Delay n' -> ("Delay", n')
			Caption n' -> ("Caption", n')
			Branch n' -> ("Branch", n')
			IdGate n' -> ("IdGate", n')

circuitDiagram :: DiagramMapM ()
circuitDiagram = do
	ip0 <- inputPosition =<< newElement0 (NotGate 0) notGateD
	ip1 <- inputPosition
		=<< newElement (Caption 0) (hLineTextD "31:16" "63:32") ip0
	_ <- newElement (NotGate 1) notGateD ip1
	connectLine (NotGate 0) (Caption 0)
	connectLine (Caption 0) (NotGate 1)
	connectLine (NotGate 1) (NotGate 1)

	ip2 <- inputPosition =<< newElement0 (NotGate 2) notGateD
	lp3 <- newElement (Branch 0) branchD ip2
	connectLine (NotGate 2) (NotGate 2)
	connectLine1 (Branch 0) (NotGate 2)

	ip3 <- inputPosition2 lp3
	lp4 <- newElement (TriGate 0) triGateD ip3
	connectLine2 (Branch 0) (TriGate 0)
	ip4 <- inputPosition1 lp4
	ip5 <- inputPosition2 lp4
	ip6 <- inputPosition =<< newElement (NotGate 3) notGateD ip4
	ip7 <- inputPosition =<< newElement (NotGate 4) notGateD ip5
	connectLine1 (TriGate 0) (NotGate 3)
	connectLine2 (TriGate 0) (NotGate 4)

	() <$ newElement (ConstGate 0) (constGateD 0x123456789abcdef0) ip6
	connectLine (NotGate 3) (ConstGate 0)

	ip8 <- inputPosition =<< newElement (Delay 0) (delayD 255) ip7
	connectLine (NotGate 4) (Delay 0)

	ip9 <- inputPosition =<< newElement (NotGate 5) notGateD ip8
	connectLine (Delay 0) (NotGate 5)

	() <$ newElement (IdGate 0) hLineD ip9
	connectLine (NotGate 5) (IdGate 0)
