{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit where

import Prelude
import qualified Prelude as P

import Control.Monad.State
-- import Data.Word
import Data.IntMap.Strict
import Data.Vector.Unboxed

import qualified Control.Monad.State as State
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as V

import CircuitTypes
import IntMapToVector
import Tools

data Circuit = Circuit {
	cctGate :: Vector BasicGateWord,
	cctWireConn :: Vector OWireInt,
	cctWireStt :: Vector BitInt8 }
	deriving Show

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit cb = (x ,) $ Circuit {
	cctGate = intMapToVector encodeBasicGate own gs,
	cctWireConn = intMapToVector encodeOWire iwn wc,
	cctWireStt = V.fromList . P.replicate iwn $ encodeBit X }
	where (	x,
		CBState {
			cbsIWireNum = iwn,
			cbsOWireNum = own,
			cbsGate = gs,
			cbsWireConn = wc } ) =
		cb `runState` initCBState

type CircuitBuilder = State CBState

data CBState = CBState {
	cbsIWireNum :: Int, cbsOWireNum :: Int,
	cbsGate :: IntMap BasicGate, cbsWireConn :: IntMap OWire }
	deriving Show

initCBState :: CBState
initCBState = CBState 0 0 IM.empty IM.empty

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
