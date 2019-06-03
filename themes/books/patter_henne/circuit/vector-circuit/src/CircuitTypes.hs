{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes where

import Data.Bits
import Data.List
import Data.Word
import Data.Int

data Bit = X | O | I deriving Show
type BitInt8 = Int8

encodeBit :: Bit -> BitInt8
encodeBit X = - 1
encodeBit O = 0
encodeBit I = 1

decodeBit :: BitInt8 -> Bit
decodeBit (- 1) = X
decodeBit 0 = O
decodeBit 1 = I
decodeBit _ = error "decodeBit: decode error"

newtype IWire = IWire { encodeIWire :: Int } deriving Show
type IWireInt = Int

decodeIWire :: IWireInt -> IWire
decodeIWire = IWire

newtype OWire = OWire { encodeOWire :: Int } deriving Show
type OWireInt = Int

decodeOWire :: OWireInt -> OWire
decodeOWire = OWire

data BasicGate
	= Delay [Bit] IWire
	| AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| ConstGate Bit
	deriving Show

gateIWires :: BasicGate -> [IWire]
gateIWires (Delay _ i) = [i]
gateIWires (AndGate i1 i2) = [i1, i2]
gateIWires (OrGate i1 i2) = [i1, i2]
gateIWires (NotGate i) = [i]
gateIWires (ConstGate _) = []

type BasicGateWord = Word64

{-

Delay    : 0xe0 - 0xff bits(2 * 16) i(24)
AndGate  : 0x00 01 i1(24) i2(24)
OrGate   : 0x00 02 i1(24) i2(24)
NotGate  : 0x00 03 00 00 01 i(24)
ConstGate: 0x00 03 00 00 02 00 00 1 b(4)

-}

encodeBasicGate :: BasicGate -> BasicGateWord
encodeBasicGate (Delay bs iw) = op .|. ln .|. pbs .|. fromIntegral (encodeIWire iw)
	where
	op = 0x7 `shiftL` 61
	ln = genericLength bs `shiftL` 56
	pbs = packBits bs `shiftR` 8
encodeBasicGate (AndGate iw1 iw2) = op .|. i1 .|. i2
	where
	op = 0x0001 `shiftL` 48
	i1 = fromIntegral (encodeIWire iw1) `shiftL` 24
	i2 = fromIntegral $ encodeIWire iw2
encodeBasicGate (OrGate iw1 iw2) = op .|. i1 .|. i2
	where
	op = 0x0002 `shiftL` 48
	i1 = fromIntegral (encodeIWire iw1) `shiftL` 24
	i2 = fromIntegral $ encodeIWire iw2
encodeBasicGate (NotGate iw) = op .|. i
	where
	op = 0x0003000001 `shiftL` 24
	i = fromIntegral $ encodeIWire iw
encodeBasicGate (ConstGate b) = op .|. packBit b
	where op = 0x0003000002000010

packBit :: Bit -> Word64
packBit X = 0x01
packBit O = 0x02
packBit I = 0x03

unpackBit :: Word64 -> Bit
unpackBit 0x01 = X
unpackBit 0x02 = O
unpackBit 0x03 = I
unpackBit w = error $ "unpackBit: unpack error: w = " ++ show w

packBits :: [Bit] -> Word64
packBits [] = zeroBits
packBits (b : bs) = packBit b `shiftL` 62 .|. packBits bs `shiftR` 2

{-

Delay    : 0xe0 - 0xff bits(2 * 16) i(24)
AndGate  : 0x00 01 i1(24) i2(24)
OrGate   : 0x00 02 i1(24) i2(24)
NotGate  : 0x00 03 00 00 01 i(24)
ConstGate: 0x00 03 00 00 02 00 00 1 b(4)

-}

decodeBasicGate :: BasicGateWord -> BasicGate
decodeBasicGate w = case op0 of
	0x07 -> uncurry Delay $ decodeDelay a0
	0x00 -> case op1 of
		0x01 -> AndGate
			(decodeIWire . fromIntegral $ a1 `shiftR` 24)
			(decodeIWire . fromIntegral $ a1 .&. 0xffffff)
		0x02 -> OrGate
			(decodeIWire . fromIntegral $ a1 `shiftR` 24)
			(decodeIWire . fromIntegral $ a1 .&. 0xffffff)
		0x03 -> case op2 of
			0x01 -> NotGate
				. decodeIWire . fromIntegral $ a2 .&. 0xffffff
			0x02 -> case op3 of
				0x01 -> ConstGate $ unpackBit a3
				_ -> error "decodeBasicGate: decode error: "
			_ -> error "decodeBasicGate: decode error"
		_ -> error "decodeBasicGate: decode error"
	_ -> error $ "decodeBasicGate: decode error: op0 == " ++ show op0
	where
	op0 = w `shiftR` 61
	op1 = w `shiftR` 48 .&. 0x3f
	op2 = w `shiftR` 24 .&. 0xffffff
	op3 = w `shiftR` 4 .&. 0x0fffff
	a0 = w .&. 0x1fffffffffffffff
	a1 = w .&. 0xffffffffffff
	a2 = w .&. 0xffffff
	a3 = w .&. 0x0f

decodeDelay :: Word64 -> ([Bit], IWire)
decodeDelay w = (
	takeBits (w `shiftR` 56) (w `shiftR` 24 .&. 0xffffffff),
	decodeIWire . fromIntegral $ w .&. 0xffffff )

{-

Delay    : 0xe0 - 0xff bits(2 * 16) i(24)

-}

nextDelay :: BasicGateWord -> BitInt8 -> (BitInt8, BasicGateWord)
nextDelay w b_ = (wordToBitInt8 b1, ot .|. bs')
	where
	ot = w .&. 0xff00000000ffffff
	ln = fromIntegral $ w `shiftR` 56 .&. 0x1f
	bs = w .&. 0x003fffffff000000
	b1 = w `shiftR` 54 .&. 0x03
	bs' = bs `shiftL` 2 .&.
		complement (0x03 `shiftL` (56 - ln * 2)) .|. 
		b `shiftL` (56 - ln * 2)
	b = bitInt8ToWord b_

bitInt8ToWord :: BitInt8 -> Word64
bitInt8ToWord (- 1) = 0x01
bitInt8ToWord 0 = 0x02
bitInt8ToWord 1 = 0x03
bitInt8ToWord _ = error "bitInt8ToWord: conversion error"

wordToBitInt8 :: Word64 -> BitInt8
wordToBitInt8 0x01 = - 1
wordToBitInt8 0x02 = 0
wordToBitInt8 0x03 = 1
wordToBitInt8 _ = error "wordToBitInt8: conversion error"

takeBits :: Word64 -> Word64 -> [Bit]
takeBits 0 _ = []
takeBits n w = unpackBit (w `shiftR` 30) : takeBits (n - 1) (w `shiftL` 2 .&. 0xffffffff)
