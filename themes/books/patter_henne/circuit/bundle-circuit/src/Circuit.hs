{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit where

import Control.Monad.State
import Data.Map.Strict

import CircuitTypes
import Tools

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit cb = (x ,) $ Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = fromList
		$ zip (gateWires =<< concat (elems gs)) (repeat $ Bits 0) }
	where
	(x, CBState { cbsGate = gs, cbsWireConn = wc }) =
		cb `runState` initCBState

makeAndGate :: BitLen -> BitPosOut -> BitPosIn -> BitPosIn ->
	CircuitBuilder (IWire, IWire, OWire)
makeAndGate ln po pi1 pi2 = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate ln po (a, pi1) (b, pi2)) o
	return (a, b, o)

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g o cbs = cbs { cbsGate = push o g $ cbsGate cbs }
