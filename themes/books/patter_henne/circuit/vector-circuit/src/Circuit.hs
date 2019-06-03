{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBit, peekOWire,
	CircuitBuilder, andGate, orGate, notGate, idGate, delay, connectWire,
	IWire, OWire, Bit(..) ) where

import Prelude
import qualified Prelude as P

import Control.Arrow
import Control.Monad.State
import Control.Monad.ST
import Data.Maybe
import Data.List
import Data.Word
import Data.IntMap.Strict
import Data.Vector.Unboxed
import Data.Vector.Unboxed.Mutable

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
	cctGate = intMapToVector 0 encodeBasicGate own gs,
	cctWireConn = intMapToVector (- 1) encodeOWire iwn wc,
	cctWireStt = V.fromList . P.replicate iwn $ encodeBit X }
	where (	x,
		CBState {
			cbsIWireNum = iwn,
			cbsOWireNum = own,
			cbsGate = gs,
			cbsWireConn = wc } ) =
		cb `runState` initCBState

step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	(ds, ows) = mapAndUpdate (checkOWire cct) gs gs in
	cct {	cctGate = ds,
		cctWireStt = V.zipWith (nextIWire cct ows) (V.fromList $ P.take (V.length wst) [0 ..]) wst }

checkOWire :: Circuit -> OWireInt -> BasicGateWord ->
	(Maybe (OWireInt, BasicGateWord), BitInt8)
checkOWire Circuit { cctWireStt = wst } ow =
	maybe (Nothing, encodeBit X) (first ((ow ,) <$>)) . calcGate wst

calcGate :: Vector BitInt8 -> BasicGateWord -> Maybe (Maybe BasicGateWord, BitInt8)
calcGate wst bgw = case branchBasicGate1 bgw of
	(0x07, _) -> case mnb of
		Just nb -> Just (Just bgw', b)
			where
			(b, bgw') = nextDelay bgw nb
		Nothing -> Nothing
		where
		mnb = wst V.!? iw2
		iw2 = iWire2FromBasicGate bgw
	(0x00, a1) -> case branchBasicGate2 a1 of
		(0x01, iws) -> case (,) <$> wst V.!? fromIntegral iw1 <*> wst V.!? fromIntegral iw2 of
			Just (a, b) -> Just (Nothing, a `andBit` b)
			Nothing -> Nothing
			where
			(iw1, iw2) = twoIWiresBasicGate iws
		(0x02, iws) -> case (,) <$> wst V.!? fromIntegral iw1 <*> wst V.!? fromIntegral iw2 of
			Just (a, b) -> Just (Nothing, a `orBit` b)
			Nothing -> Nothing
			where
			(iw1, iw2) = twoIWiresBasicGate iws
		(0x03, a2) -> case branchBasicGate3 a2 of
			(0x01, iw) -> case wst V.!? fromIntegral iw of
				Just i -> Just (Nothing, notBit i)
				Nothing -> Nothing
			_ -> error "not yet implemented"
		_ -> error "not yet implemented"
	_ -> error "not yet implemented"

nextIWire :: Circuit -> Vector BitInt8 -> IWireInt -> BitInt8 -> BitInt8
nextIWire Circuit { cctWireConn = wc } ows iw ob =
	fromMaybe ob $ (ows V.!?) =<< wc V.!? iw

setBit :: IWire -> Bit -> Circuit -> Circuit
setBit i b = setBitGen (encodeIWire i) (encodeBit b)

setBitGen :: IWireInt -> BitInt8 -> Circuit -> Circuit
setBitGen i b c = c {
	cctWireStt = runST $ do
		s <- thaw $ cctWireStt c
		write s i b
		freeze s }

peekOWire :: OWire -> Circuit -> Bit
peekOWire o = decodeBit . peekOWireGen (encodeOWire o)

peekOWireGen :: OWireInt -> Circuit -> BitInt8
peekOWireGen o Circuit { cctGate = gs, cctWireStt = wst } =
	fromJust $ snd <$> (calcGate wst =<< gs V.!? o)

type CircuitBuilder = State CBState

data CBState = CBState {
	cbsIWireNum :: Int, cbsOWireNum :: Int,
	cbsGate :: IntMap BasicGate, cbsWireConn :: IntMap OWire }
	deriving Show

initCBState :: CBState
initCBState = CBState 0 0 IM.empty IM.empty

connectWire :: OWire -> IWire -> CircuitBuilder ()
connectWire o i = State.modify $ insConn o (encodeIWire i)

insConn :: OWire -> IWireInt -> CBState -> CBState
insConn o i cbs = cbs { cbsWireConn = IM.insert i o $ cbsWireConn cbs }

sccIWireNum :: CBState -> CBState
sccIWireNum cbs = cbs { cbsIWireNum = cbsIWireNum cbs + 1 }

sccOWireNum :: CBState -> CBState
sccOWireNum cbs = cbs { cbsOWireNum = cbsOWireNum cbs + 1 }

andGate, orGate :: CircuitBuilder (IWire, IWire, OWire)
andGate = do
	(a, b, o) <- makeAndGate
	(dli, dlo) <- delay 2
	connectWire o dli
	return (a, b, dlo)

orGate = do
	(a, b, o) <- makeOrGate
	(dli, dlo) <- delay 2
	connectWire o dli
	return (a, b, dlo)

makeAndGate, makeOrGate :: CircuitBuilder (IWire, IWire, OWire)
makeAndGate = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	State.modify $ insGate (AndGate a b) o
	return (a, b, o)

makeOrGate = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	State.modify $ insGate (OrGate a b) o
	return (a, b, o)

notGate :: CircuitBuilder (IWire, OWire)
notGate = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	State.modify $ insGate (NotGate i) o
	return (i, o)

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsIWireNum sccIWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify cbsOWireNum sccOWireNum

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g (OWire o) cbs = cbs { cbsGate = IM.insert o g $ cbsGate cbs }

idGate :: CircuitBuilder (IWire, OWire)
idGate = delay 1

delay :: Word8 -> CircuitBuilder (IWire, OWire)
delay w | w > 0 = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	State.modify $ insGate (Delay ((w - 1) `genericReplicate` X) i) o
	return (i, o)
delay _ = error "0 delay is not permitted"
