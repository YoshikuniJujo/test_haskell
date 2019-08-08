{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude (mkWidth)
import Diagrams.Backend.SVG (renderSVG)

import Circuit.Diagram.Map (
	ElementIdable(..), DiagramMap,
	connectLine, getInputPos, Pos(..), Element(..),
	putElement0, putElement, putElementWithPos, execDiagramMapM )
import Circuit.Diagram.Draw
import Crypto.Hash

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteArray as BA

main :: IO ()
main = case circuitDiagram of
	Right cd -> renderSVG "simple.svg" (mkWidth 600) $ drawDiagram cd
	Left emsg -> putStrLn $ "Can't draw diagram: " ++ emsg

data Elem = NotGate Word | Caption Word | Branch Word deriving Show

instance ElementIdable Elem where
	elementIdGen e =
		BA.convert . hash @_ @SHA3_256 . BSC.pack $ pfx ++ show n
		where (pfx, n) = case e of
			NotGate n' -> ("NotGate", n')
			Caption n' -> ("Caption", n')
			Branch n' -> ("Branch", n')

circuitDiagram :: Either String DiagramMap
circuitDiagram = execDiagramMapM 15 8 $ do
	_ <- putElement0 (NotGate 0) NotGateE 2
	_ <- putElementWithPos (Caption 0) (HLineText "31:16" "63:32") (Pos 7 1)
	_ <- putElement (NotGate 1) NotGateE 11
	p0 <- head <$> getInputPos (NotGate 0)
	p1 <- head <$> getInputPos (NotGate 1)
	p100 <- head <$> getInputPos (Caption 0)
	connectLine p0 $ Caption 0
	connectLine p100 $ NotGate 1
	connectLine p1 $ NotGate 1
	_ <- putElement0 (NotGate 2) NotGateE 2
	p2 <- head <$> getInputPos (NotGate 2)
	connectLine p2 $ NotGate 2

	_ <- putElementWithPos (Branch 0) BranchE (Pos 7 6)
	p101 <- head <$> getInputPos (Branch 0)
	connectLine p101 $ NotGate 2
