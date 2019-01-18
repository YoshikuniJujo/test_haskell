{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TestGates where

import Control.Arrow

import qualified Data.Set as S
import qualified Data.Map as M

import Gates

--------------------------------------------------------------------------------

mux2 :: CreateCircuit (InWire, InWire, InWire, OutWire)
mux2 = do
	(a, s1, a1o) <- andGate
	(b, s2, a2o) <- andGate
	(xi1, xi2, c) <- orGate
	(ni, no) <- notGate
	(si, so) <- idGate
	connectWire so ni
	connectWire no s1
	connectWire so s2
	connectWire a1o xi1
	connectWire a2o xi2
	return (a, b, si, c)

getMux2Wires :: (InWire, InWire, InWire, OutWire) ->
	Circuit -> ([(String, Bit)], (String, Bit))
getMux2Wires (a, b, s, c) =
	zip ["a", "b", "s"] . M.elems
--		. filter ((`elem` [a, b, s]) . fst) . circuitState &&&
		. (`M.restrictKeys` S.fromList [a, b, s]) . circuitState &&&
	(("c" ,) . getOutWire c)

setMux2Wires ::
	(InWire, InWire, InWire, a) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setMux2Wires (a, b, s, _) (ba, bb, bs) = setBit a ba . setBit b bb . setBit s bs
