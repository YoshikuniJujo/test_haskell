{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitTypes where

import Control.Monad.State
import Data.Map.Strict
import Data.Word

import Tools

newtype IWire = IWire Word32 deriving (Show, Eq, Ord)
newtype OWire = OWire Word32 deriving (Show, Eq, Ord)

newtype Bits = Bits Word64 deriving (Show, Eq)

data Circuit = Circuit {
	cctGate :: Map OWire [BasicGate],
	cctWireConn :: Map IWire [(OWire, BitLen, BitPosIn, BitPosOut)],
	cctWireStt :: Map IWire Bits } deriving Show

type CircuitBuilder = State CBState

type BitLen = Word8
type BitPosIn = Word8
type BitPosOut = Word8

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OWire [BasicGate],
	cbsWireConn :: Map IWire [(OWire, BitLen, BitPosIn, BitPosOut)]
	} deriving Show

initCBState :: CBState
initCBState = CBState { cbsWireNum = 0, cbsGate = empty, cbsWireConn = empty }

data BasicGate
	= ConstGate BitLen Word8 Bits
	| AndGate BitLen BitPosOut (IWire, BitPosIn) (IWire, BitPosIn)
	| OrGate BitLen BitPosOut (IWire, BitPosIn) (IWire, BitPosIn)
	| NotGate BitLen BitPosOut (IWire, BitPosIn)
	deriving Show

gateWires :: BasicGate -> [IWire]
gateWires (ConstGate _ _ _) = []
gateWires (AndGate _ _ (a, _) (b, _)) = [a, b]
gateWires (OrGate _ _ (a, _) (b, _)) = [a, b]
gateWires (NotGate _ _ (i, _)) = [i]

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify cbsWireNum sccWireNum

sccWireNum :: CBState -> CBState
sccWireNum cbs = cbs { cbsWireNum = cbsWireNum cbs + 1 }
