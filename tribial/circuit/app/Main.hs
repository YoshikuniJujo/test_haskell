{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Circuit
import TestCircuit

main :: IO ()
main = let
	(ws, cct) = makeCircuit $ alu_s 64
	cct0 = setAlu_s ws I 2 I 10 3 cct in
	print . peekAlu_s ws . (!! 700) $ iterate step cct0
