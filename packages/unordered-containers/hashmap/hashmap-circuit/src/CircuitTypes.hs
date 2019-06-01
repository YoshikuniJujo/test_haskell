{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes where

import Data.Word
import Data.Hashable

data Bit = X | O | I deriving Show

andBit, orBit :: Bit -> Bit -> Bit
andBit O _ = O
andBit _ O = O
andBit I I = I
andBit _ _ = X

orBit I _ = I
orBit _ I = I
orBit O O = O
orBit _ _ = X

notBit :: Bit -> Bit
notBit O = I
notBit I = O
notBit _ = X

newtype IWire = IWire Word32 deriving (Show, Eq, Ord, Hashable)
newtype OWire = OWire Word32 deriving (Show, Eq, Ord, Hashable)

data BasicGate
	= AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| Delay [Bit] IWire
	deriving Show

gateIWires :: BasicGate -> [IWire]
gateIWires (AndGate i1 i2) = [i1, i2]
gateIWires (OrGate i1 i2) = [i1, i2]
gateIWires (NotGate i) = [i]
gateIWires (Delay _ i) = [i]
