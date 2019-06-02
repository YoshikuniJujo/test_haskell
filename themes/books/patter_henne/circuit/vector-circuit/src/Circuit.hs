{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit where

import Control.Monad.State
-- import Data.Word
import Data.IntMap.Strict
import Data.Vector.Unboxed

import qualified Control.Monad.State as State

import CircuitTypes
import Tools

data Circuit = Circuit {
	cctGate :: Vector BasicGateWord,
	cctWireConn :: Vector OWireInt,
	cctWireStt :: Vector BitInt8 }
	deriving Show

type CircuitBuilder = State CBState

data CBState = CBState {
	cbsIWireNum :: Int, cbsOWireNum :: Int,
	cbsGate :: IntMap BasicGate, cbsWireConn :: IntMap OWire }

sccIWireNum :: CBState -> CBState
sccIWireNum cbs = cbs { cbsIWireNum = cbsIWireNum cbs + 1 }

sccOWireNum :: CBState -> CBState
sccOWireNum cbs = cbs { cbsOWireNum = cbsOWireNum cbs + 1 }


makeAndGate :: CircuitBuilder (IWire, IWire, OWire)
makeAndGate = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	State.modify $ insGate (AndGate a b) o
	return (a, b, o)

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsIWireNum sccIWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify cbsOWireNum sccOWireNum

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g (OWire o) cbs = cbs { cbsGate = insert o g $ cbsGate cbs }
