{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CarryLookahead where

import Control.Monad
import Data.Word

import Circuit

generateGp :: CircuitBuilder (IWire, IWire, OWire, OWire)
generateGp = do
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(aa, ab, g) <- andGate64
	(oa, ob, p) <- orGate64
	zipWithM_ connectWire64 [aout, bout, aout, bout] [aa, ab, oa, ob]
	return (ain, bin, g, p)

{-
raiseGp :: OWire -> OWire -> IWire -> IWire -> Word8 -> CircuitBuilder ()
raiseGp gs ps rg rp p = do
	(p1g, g0, p1g0) <- andGate0
	(g1, p1g0', rgout) <- orGate0
	(p1p, p0, rpout) <- andGate0
	connectWire (ps, 1, p) (p0, 1, 0)
	connectWire (ps, 1, p + 1) (p1g, 1, 0)
	connectWire (ps, 1, p + 1) (p1p, 1, 0)
	connectWire (gs, 1, p) (g0, 1, 0)
	connectWire (gs, 1, p + 1) (g1, 1, 0)
	connectWire0 p1g0 p1g0'
	connectWire (rgout, 1, 0) (rg, 1, p `div` 2)
	connectWire (rpout, 1, 0) (rp, 1, p `div` 2)

testRaise :: CircuitBuilder (IWire, IWire, OWire, OWire)
testRaise = do
	(gin, gout) <- idGate64
	(pin, pout) <- idGate64
	(rgin, rgout) <- idGate64
	(rpin, rpout) <- idGate64
	raiseGp gout pout rgin rpin 0
	return (gin, pin, rgout, rpout)
	-}

raiseGp :: Word8 -> CircuitBuilder (IWire, IWire, OWire, OWire)
raiseGp n = do
	(gin, gout) <- idGate64
	(pin, pout) <- idGate64
	(p1g, g0, p1g0) <- andGate (64 - n) n 0 0
	(g1, p1g0', rgout) <- orGate (64 - n) n 0 0
	(p1p, p0, rpout) <- andGate (64 - n) n 0 0
	zipWithM_ connectWire64
		[pout, gout, gout, p1g0, pout, pout]
		[p1g, g0, g1, p1g0', p1p, p0]
	return (gin, pin, rgout, rpout)

raisingGp :: Word8 -> OWire -> OWire -> CircuitBuilder ([OWire], [OWire])
raisingGp 0 a b = do
	(a', b', g1, p1) <- generateGp
	zipWithM_ connectWire64 [a, b] [a', b']
	return ([g1], [p1])
{-
raisingGp 1 a b = do
	(a', b', g1, p1) <- generateGp
	(g1', p1', rg1, rp1) <- raiseGp 1
	zipWithM_ connectWire64 [a, b, g1, p1] [a', b', g1', p1']
	return ([rg1], [rp1])
	-}
raisingGp n a b = do
	(gn, pn, rgn, rpn) <- raiseGp n
	(rga, rpa) <- raisingGp (n `div` 2) a b
	let	rg : _ = rga
		rp : _ = rpa
	connectWire64 rg gn
	connectWire64 rp pn
	return (rgn : rga, rpn : rpa)

carries :: OWire -> OWire -> OWire -> CircuitBuilder (OWire, OWire)
carries ci a b = do
	(gs, ps) <- raisingGp 32 a b
	cs@(csin, csout) <- idGate64
	co <- makeCarries 0 64 gs ps cs
	connectWire (ci, 1, 0) (csin, 1, 0)
	return (csout, co)

makeCarries :: Word8 -> Word8 -> [OWire] -> [OWire] -> (IWire, OWire) -> CircuitBuilder OWire
makeCarries f t [g] [p] (_csin, csout) | f + 1 == t = generateCarry f g p csout
makeCarries f t (g : gs) (p : ps) cs@(csin, csout) = do
	co <- makeCarries f m gs ps cs
	_ <- makeCarries m t gs ps cs
	connectWire (co, 1, 0) (csin, 1, m)
	generateCarry f g p csout
	where m = (t - f) `div` 2 + f
makeCarries f t [] [] _ = error $ "makeCarries: f = " ++ show f ++ " t = " ++ show t
makeCarries _ _ _ _ _ = error "makeCarries: Oops!"

generateCarry :: Word8 -> OWire -> OWire -> OWire -> CircuitBuilder OWire
generateCarry f g p cs = do
	(p', ci, pci) <- andGate0
	(g', pci', co) <- orGate0
	connectWire (g, 1, f) (g', 1, 0)
	connectWire (p, 1, f) (p', 1, 0)
	connectWire (cs, 1, f) (ci, 1, 0)
	connectWire0 pci pci'
	return co

testCarries :: CircuitBuilder (IWire, IWire, IWire, OWire, OWire)
testCarries = do
	(ciin, ciout) <- idGate0
	(ain, aout) <- idGate64
	(bin, bout) <- idGate64
	(cs, co) <- carries ciout aout bout
	return (ciin, ain, bin, cs, co)
