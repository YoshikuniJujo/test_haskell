{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBit, peekIWire, peekOWire,
	CircuitBuilder, andGate, orGate, notGate, idGate, connectWire,
	InWire, OutWire, Bit(..) ) where

import Control.Arrow ((***))
import Control.Monad.State (
	MonadState, State, StateType, runState, gets, modify)
import Data.Maybe (fromMaybe, fromJust, catMaybes, mapMaybe)
import Data.List (genericReplicate)
import Data.Word (Word8, Word32)

import Data.Map (Map, (!?), (!))
import qualified Data.Map as M

data Circuit = Circuit {
	cctGate :: Map OutWire BasicGate,
	cctWireConn :: Map InWire OutWire,
	cctWireStt :: Map InWire Bit } deriving Show

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit cct = (x, Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = M.fromList $ zip (gateWires =<< M.elems gs) (repeat O) })
	where
	(x, CBState { cbsGate = gs, cbsWireConn = wc }) =
		cct `runState` initCBState

step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	(ows, ds) = checkOWires cct $ M.keys gs in
	cct {	cctWireStt = M.mapWithKey (nextIWire cct ows) wst,
		cctGate = foldr (uncurry M.insert) gs ds }

nextIWire :: Circuit -> Map OutWire Bit -> InWire -> Bit -> Bit
nextIWire Circuit { cctWireConn = wc } ows iw ob =
	fromMaybe ob $ (ows !?) =<< wc !? iw

checkOWires :: Circuit -> [OutWire] -> (Map OutWire Bit, [(OutWire, BasicGate)])
checkOWires cs ows = (foldr (uncurry M.insert) M.empty *** catMaybes) $ unzip
	$ map (\(a, (b, c)) -> ((a, b), (a ,) <$> c))
		$ zip ows $ mapMaybe (checkOWire cs) ows

checkOWire :: Circuit -> OutWire -> Maybe (Bit, Maybe BasicGate)
checkOWire Circuit { cctGate = gs, cctWireStt = wst } ow =
	calcGate wst =<< gs !? ow

calcGate :: Map InWire Bit -> BasicGate -> Maybe (Bit, Maybe BasicGate)
calcGate wst (AndGate iw1 iw2) =
	((, Nothing) .) . andBit <$> wst !? iw1 <*> wst !? iw2
calcGate wst (OrGate iw1 iw2) =
	((, Nothing) .) . orBit <$> wst !? iw1 <*> wst !? iw2
calcGate wst (NotGate niw) = (, Nothing) . notBit <$> wst !? niw
calcGate wst (Delay [] iw) = (, Nothing) <$> wst !? iw
calcGate wst (Delay (b : bs) iw) =
	(b ,) . Just . (`Delay` iw) . (bs ++) . (: []) <$> wst !? iw

setBit :: InWire -> Bit -> Circuit -> Circuit
setBit w b c = c { cctWireStt = M.insert w b $ cctWireStt c }

peekIWire :: InWire -> Circuit -> Bit
peekIWire iw Circuit { cctWireStt = cs } = cs ! iw

peekOWire :: OutWire -> Circuit -> Bit
peekOWire = ((fst . fromJust) .) . flip checkOWire

type CircuitBuilder = State CBState

makeInWire :: CircuitBuilder InWire
makeInWire = InWire <$> getModify cbsWireNum sccWireNum

makeOutWire :: CircuitBuilder OutWire
makeOutWire = OutWire <$> getModify cbsWireNum sccWireNum

connectWire :: OutWire -> InWire -> CircuitBuilder ()
connectWire ow iw = modify $ insConn ow iw

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OutWire BasicGate,
	cbsWireConn :: Map InWire OutWire
	} deriving Show

initCBState :: CBState
initCBState = CBState {
	cbsWireNum = 0, cbsWireConn = M.empty, cbsGate = M.empty }

sccWireNum :: CBState -> CBState
sccWireNum ccs = ccs { cbsWireNum = cbsWireNum ccs + 1 }

insGate :: BasicGate -> OutWire -> CBState -> CBState
insGate g ow css = css { cbsGate = M.insert ow g $ cbsGate css }

insConn :: OutWire -> InWire -> CBState -> CBState
insConn ow iw css = css { cbsWireConn = M.insert iw ow $ cbsWireConn css }

data BasicGate
	= AndGate InWire InWire | OrGate InWire InWire | NotGate InWire
	| Delay [Bit] InWire deriving Show

gateWires :: BasicGate -> [InWire]
gateWires (AndGate iw1 iw2) = [iw1, iw2]
gateWires (OrGate iw1 iw2) = [iw1, iw2]
gateWires (NotGate iw) = [iw]
gateWires (Delay _ iw) = [iw]

andGate, orGate :: CircuitBuilder (InWire, InWire, OutWire)
andGate = do
	(iw1, iw2, ow) <- makeAndGate
	(diw, dow) <- makeDelay 9
	connectWire ow diw
	return (iw1, iw2, dow)

orGate = do
	(iw1, iw2, ow) <- makeOrGate
	(diw, dow) <- makeDelay 9
	connectWire ow diw
	return (iw1, iw2, dow)

notGate :: CircuitBuilder (InWire, OutWire)
notGate = do
	(iw, ow) <- makeNotGate
	(diw, dow) <- makeDelay 4
	connectWire ow diw
	return (iw, dow)

idGate :: CircuitBuilder (InWire, OutWire)
idGate = makeDelay 5

makeAndGate, makeOrGate :: CircuitBuilder (InWire, InWire, OutWire)
makeAndGate = do
	(iw1, iw2, ow) <- (,,) <$> makeInWire <*> makeInWire <*> makeOutWire
	modify $ insGate (AndGate iw1 iw2) ow
	return (iw1, iw2, ow)

makeOrGate = do
	(iw1, iw2, ow) <- (,,) <$> makeInWire <*> makeInWire <*> makeOutWire
	modify $ insGate (OrGate iw1 iw2) ow
	return (iw1, iw2, ow)

makeNotGate :: CircuitBuilder (InWire, OutWire)
makeNotGate = do
	(iw, ow) <- (,) <$> makeInWire <*> makeOutWire
	modify $ insGate (NotGate iw) ow
	return (iw, ow)

makeDelay :: Word8 -> CircuitBuilder (InWire, OutWire)
makeDelay w = do
	(iw, ow) <- (,) <$> makeInWire <*> makeOutWire
	modify $ insGate (Delay (genericReplicate (w - 1) O) iw) ow
	return (iw, ow)

newtype InWire = InWire Word32 deriving (Show, Eq, Ord)
newtype OutWire = OutWire Word32 deriving (Show, Eq, Ord)

data Bit = O | I deriving (Show, Eq)

andBit, orBit :: Bit -> Bit -> Bit
andBit I I = I
andBit _ _ = O

orBit O O = O
orBit _ _ = I

notBit :: Bit -> Bit
notBit O = I
notBit I = O

--------------------------------------------------------------------------------

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m
