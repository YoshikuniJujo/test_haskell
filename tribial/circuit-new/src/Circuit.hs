{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, CircuitBuilder, makeCircuit, step, setBit, peekOWire,
	IWire, OWire, Bit(..), andGate, orGate, notGate, idGate, triGate, connectWire
	) where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Map (Map, (!?))
import Data.Word

import qualified Data.Map as M

import Tools

newtype IWire = IWire Word32 deriving (Show, Eq, Ord)
newtype OWire = OWire Word32 deriving (Show, Eq, Ord)

data Bit = X | Z | O | I deriving (Show, Eq)

andBit, orBit :: Bit -> Bit -> Bit
andBit i1 i2
	| i1 == I && i2 == I = I
	| i1 == O || i2 == O = O
	| otherwise = X

orBit i1 i2
	| i1 == O && i2 == O = O
	| i1 == I || i2 == I = I
	| otherwise = X

notBit :: Bit -> Bit
notBit O = I
notBit I = O
notBit _ = X

triBit :: Bit -> Bit -> Bit
triBit I b = b
triBit O _ = Z
triBit _ _ = X

data Circuit = Circuit {
	cctGate :: Map OWire BasicGate,
	cctWireConn :: Map IWire [OWire],
	cctWireStt :: Map IWire Bit } deriving Show

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit cb = (x, Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = M.fromList $ zip (gateWires =<< M.elems gs) (repeat X) })
	where
	(x, CBState { cbsGate = gs, cbsWireConn = wc }) =
		cb `runState` initCBState

step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	(ds, ows) = mapAndCollect (checkOWire cct) gs in
	cct {	cctGate = foldr (uncurry M.insert) gs ds,
		cctWireStt = M.mapWithKey (nextIWire cct ows) wst }

setBit :: IWire -> Bit -> Circuit -> Circuit
setBit w b c = c { cctWireStt = M.insert w b $ cctWireStt c }

peekOWire :: OWire -> Circuit -> Bit
peekOWire w Circuit { cctGate = gs, cctWireStt = wst } =
	fromJust $ snd <$> (calcGate wst =<< gs !? w)

checkOWire :: Circuit -> OWire -> BasicGate -> (Maybe (OWire, BasicGate), Bit)
checkOWire Circuit { cctWireStt = wst } ow =
	maybe (Nothing, X) (first ((ow, ) <$>)) . calcGate wst

calcGate :: Map IWire Bit -> BasicGate -> Maybe (Maybe BasicGate, Bit)
calcGate wst (AndGate i1 i2) =
	((Nothing ,) .) . andBit <$> wst !? i1 <*> wst !? i2
calcGate wst (OrGate i1 i2) =
	((Nothing ,) .) . orBit <$> wst !? i1 <*> wst !? i2
calcGate wst (NotGate i) = (Nothing ,) . notBit <$> wst !? i
calcGate wst (TriGate i1 i2) =
	((Nothing ,) .) . triBit <$> wst !? i1 <*> wst !? i2
calcGate wst (Delay [] i) = (Nothing ,) <$> wst !? i
calcGate wst (Delay (b : bs) i) =
	(, b) . Just . (`Delay` i) . (bs ++) . (: []) <$> wst !? i

nextIWire :: Circuit -> Map OWire Bit -> IWire -> Bit -> Bit
nextIWire Circuit { cctWireConn = wc } ows iw ob = fromMaybe ob
	$ procOWire <$> (mapM (ows !?) =<< wc !? iw)

procOWire :: [Bit] -> Bit
procOWire [] = X
procOWire os = case dropWhile (== Z) os of
	[] -> Z
	b : _ -> b

type CircuitBuilder = State CBState

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OWire BasicGate,
	cbsWireConn :: Map IWire [OWire] } deriving Show

initCBState :: CBState
initCBState = CBState {
	cbsWireNum = 0, cbsWireConn = M.empty, cbsGate = M.empty }

data BasicGate
	= AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| TriGate IWire IWire | Delay [Bit] IWire deriving Show

gateWires :: BasicGate -> [IWire]
gateWires (AndGate i1 i2) = [i1, i2]
gateWires (OrGate i1 i2) = [i1, i2]
gateWires (NotGate i) = [i]
gateWires (TriGate i1 i2) = [i1, i2]
gateWires (Delay _ i) = [i]

connectWire :: OWire -> IWire -> CircuitBuilder ()
connectWire o i = modify $ insConn o i

andGate, orGate :: CircuitBuilder (IWire, IWire, OWire)
andGate = do
	(i1, i2, o) <- makeAndGate
	(dli, dlo) <- delay 2
	connectWire o dli
	return (i1, i2, dlo)

orGate = do
	(i1, i2, o) <- makeOrGate
	(dli, dlo) <- delay 2
	connectWire o dli
	return (i1, i2, dlo)

notGate, idGate :: CircuitBuilder (IWire, OWire)
notGate = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (NotGate i) o
	return (i, o)

idGate = delay 1

triGate :: CircuitBuilder (IWire, IWire, OWire)
triGate = do
	(i1, i2, o) <- makeTriGate
	(dli, dlo) <- delay 2
	connectWire o dli
	return (i1, i2, dlo)

makeAndGate, makeOrGate, makeTriGate :: CircuitBuilder (IWire, IWire, OWire)
makeAndGate = do
	(i1, i2, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate i1 i2) o
	return (i1, i2, o)

makeOrGate = do
	(i1, i2, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (OrGate i1 i2) o
	return (i1, i2, o)

makeTriGate = do
	(i1, i2, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (TriGate i1 i2) o
	return (i1, i2, o)

delay :: Word8 -> CircuitBuilder (IWire, OWire)
delay dr | dr < 1 = error "0 delay is not permitted"
delay dr = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (Delay ((dr - 1) `genericReplicate` X) i) o
	return (i, o)

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify cbsWireNum sccWireNum

sccWireNum :: CBState -> CBState
sccWireNum cbs = cbs { cbsWireNum = cbsWireNum cbs + 1 }

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g o cbs = cbs { cbsGate = M.insert o g $ cbsGate cbs }

insConn :: OWire -> IWire -> CBState -> CBState
insConn o i cbs = cbs { cbsWireConn = M.insert i (o : indexOrEmpty (cbsWireConn cbs) i) $ cbsWireConn cbs }

indexOrEmpty :: Ord k => Map k [v] -> k -> [v]
indexOrEmpty = (fromMaybe [] .) . (!?)
