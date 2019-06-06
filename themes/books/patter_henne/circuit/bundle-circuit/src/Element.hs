{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Element where

import Control.Arrow
import Data.Word

import Circuit
import Tools

nandGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
nandGate ln pi1 pi2 po = do
	(a, b, o) <- andGate ln pi1 pi2 po
	(ni, no) <- notGate ln po po
	connectWire (o, ln, po) (ni, ln, po)
	return (a, b, no)

xorGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
xorGate ln pi1 pi2 po = do
	(ain, aout) <- idGate ln pi1 pi1
	(bin, bout) <- idGate ln pi2 pi2
	(oa, ob, oo) <- orGate ln pi1 pi2 po
	(naa, nab, nao) <- nandGate ln pi1 pi2 po
	(aa, ab, ao) <- andGate ln po po po
	connectWire (aout, ln, pi1) (oa, ln, pi1)
	connectWire (bout, ln, pi2) (ob, ln, pi2)
	connectWire (aout, ln, pi1) (naa, ln, pi1)
	connectWire (bout, ln, pi2) (nab, ln, pi2)
	connectWire (oo, ln, po) (aa, ln, po)
	connectWire (nao, ln, po) (ab, ln, po)
	return (ain, bin, ao)

orGate3 :: BitLen -> BitPosIn -> CircuitBuilder ((IWire, IWire, IWire), OWire)
orGate3 l p = first listToTuple3 <$> multiple orGate 3 l p

xorGate3 :: BitLen -> BitPosIn -> CircuitBuilder ((IWire, IWire, IWire), OWire)
xorGate3 l p = first listToTuple3 <$> multiple xorGate 3 l p

multiple ::
	(BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
			CircuitBuilder (IWire, IWire, OWire)) ->
		Word8 -> BitLen -> BitPosIn -> CircuitBuilder ([IWire], OWire)
multiple _ n _ _ | n < 1 = error "Oops!"
multiple _ 1 l p = first (: []) <$> idGate l p p
multiple g 2 l p = (\(i1, i2, o) -> ([i1, i2], o)) <$> g l p p p
multiple g n l p = do
	(is1, o1) <- multiple g (n `div` 2) l p
	(is2, o2) <- multiple g (n - n `div` 2) l p
	(i1, i2, o) <- g l p p p
	connectWire (o1, l, p) (i1, l, p)
	connectWire (o2, l, p) (i2, l, p)
	return (is1 ++ is2, o)
