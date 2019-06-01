{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.Word

import CircuitTypes
import Dictionary
import Tools

data Circuit g c s = Circuit {
	cctGate :: g BasicGate,
	cctWireConn :: c OWire,
	cctWireStt :: s Bit }

instance (Show (g BasicGate), Show (c OWire), Show (s Bit)) => Show (Circuit g c s) where
	show (Circuit g c s) =
		"(Circuit (" ++ show g ++ ") (" ++ show c ++ ") (" ++ show s ++ "))"

makeCircuit :: (
	IsDictionary g, IsDictionary c,
	IsDictionary s , Key s ~ IWire ) =>
		CircuitBuilder g c a -> (a, Circuit g c s)
makeCircuit cb = (x ,) $ Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = fromList $ zip (gateIWires =<< elems gs) (repeat X) }
	where (x, CBState { cbsGate = gs, cbsWireConn = wc }) =
		cb `runState` initCBState

step :: (
	IsDictionary g, Key g ~ OWire,
	IsDictionary c, Key c ~ IWire,
	IsDictionary s, Key s ~ IWire ) =>
	Circuit g c s -> Circuit g c s
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	(ds, ows) = mapAndCollect (checkOWire cct) gs in
	cct {	cctGate = foldr (uncurry insert) gs ds,
		cctWireStt = mapWithKey (nextIWire cct ows) wst }

checkOWire :: (IsDictionary s, Key s ~ IWire ) =>
	Circuit g c s ->  OWire -> BasicGate -> (Maybe (OWire, BasicGate), Bit)
checkOWire Circuit { cctWireStt = wst } ow =
	maybe (Nothing, O) (first ((ow ,) <$>)) . calcGate wst

calcGate :: (IsDictionary s, Key s ~ IWire ) =>
	s Bit -> BasicGate -> Maybe (Maybe BasicGate, Bit)
calcGate wst (AndGate iw1 iw2) =
	((Nothing ,) .) . andBit <$> wst !? iw1 <*> wst !? iw2
calcGate wst (OrGate iw1 iw2) =
	((Nothing ,) .) . orBit <$> wst !? iw1 <*> wst !? iw2
calcGate wst (NotGate iw) = (Nothing ,) . notBit <$> wst !? iw
calcGate wst (Delay [] iw) = (Nothing ,) <$> wst !? iw
calcGate wst (Delay (b : bs) iw) =
	(, b) . Just . (`Delay` iw) . (bs ++) . (: []) <$> wst !? iw

nextIWire :: (
	IsDictionary c, Key c ~ IWire,
	IsDictionary ows, Key ows ~ OWire) =>
	Circuit g c s -> ows Bit -> IWire -> Bit -> Bit
nextIWire Circuit { cctWireConn = wc } ows iw ob =
	fromMaybe ob $ (ows !?) =<< wc !? iw
	
type CircuitBuilder g c = State (CBState g c)

data CBState g c = CBState {
	cbsWireNum :: Word32,
	cbsGate :: g BasicGate,
	cbsWireConn :: c OWire }

instance (Show (g BasicGate), Show (c OWire)) => Show (CBState g c) where
	show (CBState n g c) =
		"(CBState (" ++ show n ++ ") (" ++ show g ++ ") (" ++ show c ++ "))"

initCBState :: (IsDictionary g, IsDictionary c) => CBState g c
initCBState = CBState { cbsWireNum = 0, cbsGate = empty, cbsWireConn = empty }

connectWire :: (IsDictionary c, Key c ~ IWire) =>
	OWire -> IWire -> CircuitBuilder g c ()
connectWire o i = modify $ insConn o i

insConn :: (IsDictionary c, Key c ~ IWire) =>
	OWire -> IWire -> CBState g c -> CBState g c
insConn o i cbs = cbs { cbsWireConn = insert i o $ cbsWireConn cbs }

notGate :: (IsDictionary g, Key g ~ OWire) =>
	CircuitBuilder g c (IWire, OWire)
notGate = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (NotGate i) o
	return (i, o)

makeAndGate, makeOrGate :: (IsDictionary g, Key g ~ OWire) =>
	CircuitBuilder g c (IWire, IWire, OWire)
makeAndGate = do
	(i1, i2, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate i1 i2) o
	return (i1, i2, o)

makeOrGate = do
	(i1, i2, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (OrGate i1 i2) o
	return (i1, i2, o)

makeIWire :: CircuitBuilder g c IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder g c OWire
makeOWire = OWire <$> getModify cbsWireNum sccWireNum

sccWireNum :: CBState g c -> CBState g c
sccWireNum cbs = cbs { cbsWireNum = cbsWireNum cbs + 1 }

insGate :: (IsDictionary g, Key g ~ OWire) =>
	BasicGate -> OWire -> CBState g c -> CBState g c
insGate g o cbs = cbs { cbsGate = insert o g $ cbsGate cbs }
