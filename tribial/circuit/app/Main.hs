{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Circuit
import TestCircuit

main :: IO ()
main = let
	(ws, cct) = makeCircuit $ alu_riscv 64
	cct0 = setAlu_riscv ws O I 2 10 3 cct in
	print . peekAlu_riscv ws . (!! 700) $ iterate step cct0
