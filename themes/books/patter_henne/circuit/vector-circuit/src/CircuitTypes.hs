{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes where

import Data.Bits
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

newtype OWire = OWire Int deriving Show
type OWireInt = Int

-- encodeIWire :: IWire -> IWire Int
-- encodeIWire

data BasicGate
	= ConstGate Bit
	| AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| Delay [Bit] IWire
	deriving Show

type BasicGateWord = Word64

encodeBasicGate :: BasicGate -> Word64
encodeBasicGate = undefined

decodeBasicGate :: Word64 -> BasicGate
decodeBasicGate = undefined
