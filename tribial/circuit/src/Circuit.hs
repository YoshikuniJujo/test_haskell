{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, makeCircuit, step, setBit, peekIWire, peekOWire,
	CircuitBuilder, andGate, orGate, notGate, idGate, connectWire,
	IWire, OWire, Bit(..) ) where

import Control.Arrow (first)
import Control.Monad.State (
	MonadState, State, StateType, runState, gets, modify)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (genericReplicate)
import Data.Word (Word8, Word32)

import Data.Map (Map, (!?), (!))
import qualified Data.Map as M

data Circuit = Circuit {
	cctGate :: Map OWire BasicGate,
	cctWireConn :: Map IWire OWire,
	cctWireStt :: Map IWire Bit } deriving Show

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit cct = (x, Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = M.fromList $ zip (gateWires =<< M.elems gs) (repeat O) })
	where
	(x, CBState { cbsGate = gs, cbsWireConn = wc }) =
		cct `runState` initCBState

step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	(ds, ows) = mapAndCollect (checkOWire cct) gs in
	cct {	cctGate = foldr (uncurry M.insert) gs ds,
		cctWireStt = M.mapWithKey (nextIWire cct ows) wst }

checkOWire :: Circuit -> OWire -> BasicGate -> (Maybe (OWire, BasicGate), Bit)
checkOWire Circuit { cctWireStt = wst } ow =
	maybe (Nothing, O) (first ((ow ,) <$>)) . calcGate wst

nextIWire :: Circuit -> Map OWire Bit -> IWire -> Bit -> Bit
nextIWire Circuit { cctWireConn = wc } ows iw ob =
	fromMaybe ob $ (ows !?) =<< wc !? iw

calcGate :: Map IWire Bit -> BasicGate -> Maybe (Maybe BasicGate, Bit)
calcGate wst (AndGate iw1 iw2) =
	((Nothing ,) .) . andBit <$> wst !? iw1 <*> wst !? iw2
calcGate wst (OrGate iw1 iw2) =
	((Nothing ,) .) . orBit <$> wst !? iw1 <*> wst !? iw2
calcGate wst (NotGate iw) = (Nothing ,) . notBit <$> wst !? iw
calcGate wst (Delay [] iw) = (Nothing ,) <$> wst !? iw
calcGate wst (Delay (b : bs) iw) =
	(, b) . Just . (`Delay` iw) . (bs ++) . (: []) <$> wst !? iw

setBit :: IWire -> Bit -> Circuit -> Circuit
setBit iw b c = c { cctWireStt = M.insert iw b $ cctWireStt c }

peekIWire :: IWire -> Circuit -> Bit
peekIWire iw Circuit { cctWireStt = cs } = cs ! iw

peekOWire :: OWire -> Circuit -> Bit
peekOWire ow Circuit { cctGate = gs, cctWireStt = wst } =
	fromJust $ snd <$> (calcGate wst =<< gs !? ow)

type CircuitBuilder = State CBState

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify cbsWireNum sccWireNum

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify cbsWireNum sccWireNum

connectWire :: OWire -> IWire -> CircuitBuilder ()
connectWire ow iw = modify $ insConn ow iw

data CBState = CBState {
	cbsWireNum :: Word32,
	cbsGate :: Map OWire BasicGate,
	cbsWireConn :: Map IWire OWire
	} deriving Show

initCBState :: CBState
initCBState = CBState {
	cbsWireNum = 0, cbsWireConn = M.empty, cbsGate = M.empty }

sccWireNum :: CBState -> CBState
sccWireNum ccs = ccs { cbsWireNum = cbsWireNum ccs + 1 }

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g ow css = css { cbsGate = M.insert ow g $ cbsGate css }

insConn :: OWire -> IWire -> CBState -> CBState
insConn ow iw css = css { cbsWireConn = M.insert iw ow $ cbsWireConn css }

data BasicGate
	= AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| Delay [Bit] IWire deriving Show

gateWires :: BasicGate -> [IWire]
gateWires (AndGate iw1 iw2) = [iw1, iw2]
gateWires (OrGate iw1 iw2) = [iw1, iw2]
gateWires (NotGate iw) = [iw]
gateWires (Delay _ iw) = [iw]

andGate, orGate :: CircuitBuilder (IWire, IWire, OWire)
andGate = do
	(iw1, iw2, ow) <- makeAndGate
	(diw, dow) <- makeDelay 2
	connectWire ow diw
	return (iw1, iw2, dow)

orGate = do
	(iw1, iw2, ow) <- makeOrGate
	(diw, dow) <- makeDelay 2
	connectWire ow diw
	return (iw1, iw2, dow)

notGate :: CircuitBuilder (IWire, OWire)
notGate = do
	(iw, ow) <- makeNotGate
	(diw, dow) <- makeDelay 1
	connectWire ow diw
	return (iw, dow)

idGate :: CircuitBuilder (IWire, OWire)
idGate = makeDelay 1

makeAndGate, makeOrGate :: CircuitBuilder (IWire, IWire, OWire)
makeAndGate = do
	(iw1, iw2, ow) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate iw1 iw2) ow
	return (iw1, iw2, ow)

makeOrGate = do
	(iw1, iw2, ow) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (OrGate iw1 iw2) ow
	return (iw1, iw2, ow)

makeNotGate :: CircuitBuilder (IWire, OWire)
makeNotGate = do
	(iw, ow) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (NotGate iw) ow
	return (iw, ow)

makeDelay :: Word8 -> CircuitBuilder (IWire, OWire)
makeDelay w | w > 0 = do
	(iw, ow) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (Delay ((w - 1) `genericReplicate` O) iw) ow
	return (iw, ow)
makeDelay _ = error "0 delay is not permitted"

newtype IWire = IWire Word32 deriving (Show, Eq, Ord)
newtype OWire = OWire Word32 deriving (Show, Eq, Ord)

data Bit = O | I deriving (Show, Eq)

andBit, orBit :: Bit -> Bit -> Bit
andBit I I = I; andBit _ _ = O
orBit O O = O; orBit _ _ = I

notBit :: Bit -> Bit
notBit O = I; notBit I = O

--------------------------------------------------------------------------------

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

mapAndCollect :: (k -> v1 -> (Maybe a, v2)) -> Map k v1 -> ([a], Map k v2)
mapAndCollect f = M.mapAccumWithKey (\xs -> (first (maybe xs (: xs)) .) . f) []
