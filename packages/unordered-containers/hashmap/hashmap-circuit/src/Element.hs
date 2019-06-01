{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Control.Monad
import Data.Word

import Circuit
import Tools

orGate3 :: CircuitBuilderConstraint g c => CircuitBuilder g c ((IWire, IWire, IWire), OWire)
orGate3 = first listToTuple3 <$> multiple orGate 3

xorGate :: CircuitBuilderConstraint g c => CircuitBuilder g c (IWire, IWire, OWire)
xorGate = do
	(ain, aout) <- idGate
	(bin, bout) <- idGate
	(aa, ab, ao) <- andGate
	(oa, ob, oo) <- orGate
	(ni, no) <- notGate
	(r1, r2, r) <- andGate
	zipWithM_ connectWire
		[aout, bout, ao, no, aout, bout, oo]
		[aa, ab, ni, r1, oa, ob, r2]
	return (ain, bin, r)

xorGate3 :: CircuitBuilderConstraint g c => CircuitBuilder g c ((IWire, IWire, IWire), OWire)
xorGate3 = first listToTuple3 <$> multiple xorGate 3

multiple :: CircuitBuilderConstraint g c => CircuitBuilder g c (IWire, IWire, OWire) ->
	Word8 -> CircuitBuilder g c ([IWire], OWire)
multiple _ n | n < 1 = error "Oops!"
multiple _ 1 = first (: []) <$> idGate
multiple g 2 = (\(a, b, o) -> ([a, b], o)) <$> g
multiple g n = do
	(a, oa) <- multiple g (n `div` 2)
	(b, ob) <- multiple g (n - n `div` 2)
	(ga, gb, o) <- g
	zipWithM_ connectWire [oa, ob] [ga, gb]
	return (a ++ b, o)
