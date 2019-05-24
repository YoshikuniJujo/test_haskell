{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import RiscvAlu
import Circuit
import Tools

main :: IO ()
main = let
	(ws, cct) = makeCircuit $ riscvAlu 6
	cct0 = setBitsRiscvAlu ws 0b0000 0xff00 0x0ff0 cct
	cct1 = setBitsRiscvAlu ws 0b0001 0xff00 0x0ff0 cct
	cct2 = setBitsRiscvAlu ws 0b0010 0xff00 0x0ff0 cct
	cct3 = setBitsRiscvAlu ws 0b0110 0xff00 0x0ff0 cct
	cct4 = setBitsRiscvAlu ws 0b0111 0xff00 0x0ff0 cct
	cct5 = setBitsRiscvAlu ws 0b0111 0x0ff0 0xff00 cct
	cct6 = setBitsRiscvAlu ws 0b1100 0x0ff0 0xff00 cct in do
		putStrLn $ "AND\t0xff00 0x0ff0 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct0)
		putStrLn $ "OR\t0xff00 0x0ff0 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct1)
		putStrLn $ "add\t0xff00 0x0ff0 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct2)
		putStrLn $ "sub\t0xff00 0x0ff0 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct3)
		putStrLn $ "slt\t0xff00 0x0ff0 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct4)
		putStrLn $ "slt\t0x0ff0 0xff00 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct5)
		putStrLn $ "NOR\t0xff00 0x0ff0 : " ++ show (peekBitsRiscvAlu ws $ run 150 cct6)
