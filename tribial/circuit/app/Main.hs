{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Circuit
import TestCircuit

main :: IO ()
main = let
	(ws, cct) = makeCircuit $ riscv_alu 64
	cct0 = setRiscvAlu ws 6 10 3 cct in
	print . peekRiscvAlu ws . (!! 700) $ iterate step cct0
