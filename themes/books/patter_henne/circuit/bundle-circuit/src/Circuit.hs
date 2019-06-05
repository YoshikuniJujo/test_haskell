{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit where

import Prelude
import qualified Prelude as P

import Control.Monad.State
import Data.Maybe
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

{-
step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	ows = mapWithKey (checkOWire cct) gs in
	cct { cctWireStt = mapWithKey (nextIWire cct ows) wst }
	-}

setBits :: IWire -> Bits -> Circuit -> Circuit
setBits w bs c = c { cctWireStt = insert w bs $ cctWireStt c }

checkOWire :: Circuit -> [BasicGate] -> Bits
checkOWire Circuit { cctWireStt = wst } = P.foldr (calcGate wst) (Bits 0)

calcGate :: Map IWire Bits -> BasicGate -> Bits -> Bits
calcGate wst (AndGate ln po (i1, pi1) (i2, pi2)) = fromJust
	$ andBits ln po <$> ((, pi1) <$> wst !? i1) <*> ((, pi2) <$> wst !? i2)

{-
nextIWire :: Circuit -> Map OWire Bits -> IWire -> Bits -> Bits
nextIWire Circuit { cctWireConn = wc } ows iw ob =
	fromMaybe ob $ (ows !?) =<< wc !? iw
	-}

andGate :: BitLen -> BitPosOut -> BitPosIn -> BitPosIn ->
	CircuitBuilder (IWire, IWire, OWire)
andGate ln po pi1 pi2 = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate ln po (a, pi1) (b, pi2)) o
	return (a, b, o)

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g o cbs = cbs { cbsGate = push o g $ cbsGate cbs }
