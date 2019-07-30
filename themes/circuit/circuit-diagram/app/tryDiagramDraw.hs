{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG

import Circuit.Diagram.Pictures

main :: IO ()
main = do
	renderSVG "sample.svg" (mkWidth 400) notGateD
	renderSVG "sample2.svg" (mkWidth 600) andGateD
	renderSVG "sample3.svg" (mkWidth 600) orGateD
