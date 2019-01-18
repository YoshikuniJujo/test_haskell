{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gates (
	Circuit, CreateCircuit, InWire, OutWire,
	createCircuit, circuitState,
	Bit(..),
	connectWire,
	andGate, orGate, notGate, idGate,
	setBit, nextWire, nextCircuit2, getOutWire ) where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Word

import Data.Map (Map)
import qualified Data.Map as M

createCircuit :: CreateCircuit a -> (a, Circuit)
createCircuit cc = let
	(x, CreateCircuitStt {
		wireConnectionStt = wc,
		basicGatesStt = bg
		}) = cc `runState` initialCreateCircuitStt in
	(x, Circuit {
		wireConnection = wc,
		basicGates = bg,
		circuitState = M.fromList $ zip
			(concatMap getGateWires $ M.elems bg)
			(repeat O) })

setBit :: InWire -> Bit -> Circuit -> Circuit
setBit w b c = c { circuitState = M.insert w b $ circuitState c }

nextCircuit2 :: Circuit -> Circuit
nextCircuit2 cct@Circuit {
	circuitState = cs, basicGates = bg } = let
	(ows, ds) = nextOutWires (M.keys bg) cct
	ncs = map (flip (nextInWire ows) cct) $ M.keys cs in
	cct {	circuitState = M.fromList $ zip (M.keys cs) ncs,
		basicGates = foldr (uncurry M.insert) bg ds }

calcGateResult :: Map InWire Bit -> BasicGate -> (Bit, Maybe BasicGate)
calcGateResult cs (AndGate iw1 iw2) =
	(andBit (fromJust $ M.lookup iw1 cs) (fromJust $ M.lookup iw2 cs), Nothing)
calcGateResult cs (OrGate iw1 iw2) =
	(orBit (fromJust $ M.lookup iw1 cs) (fromJust $ M.lookup iw2 cs), Nothing)
calcGateResult cs (NotGate niw) =
	(flipBit . fromJust $ M.lookup niw cs, Nothing)
calcGateResult cs (Delay [] iw) = (fromJust $ M.lookup iw cs, Nothing)
calcGateResult cs (Delay (b : bs) iw) =
	(b, Just $ Delay (bs ++ [fromJust $ M.lookup iw cs]) iw)

nextOutWires ::
	[OutWire] -> Circuit -> (Map OutWire Bit, [(OutWire, BasicGate)])
nextOutWires ows cs = (foldr (uncurry M.insert) M.empty *** catMaybes) $ unzip
	$ map (\(a, (b, c)) -> ((a, b), (a ,) <$> c))
		$ zip ows $ map (`nextOutWire` cs) ows

nextOutWire :: OutWire -> Circuit -> (Bit, Maybe BasicGate)
nextOutWire ow Circuit { basicGates = bg, circuitState = cs } =
	case M.lookup ow bg of
		Just g -> calcGateResult cs g
		Nothing -> error "Oops!"

nextInWire :: Map OutWire Bit -> InWire -> Circuit -> Bit
nextInWire ows iw Circuit {
	wireConnection = wc, circuitState = cs } = case M.lookup iw wc of
	Just ow -> fromJust $ M.lookup ow ows
	Nothing -> fromJust $ M.lookup iw cs

nextWire :: InWire -> Circuit -> (Bit, Maybe (OutWire, BasicGate))
nextWire iw Circuit {
	wireConnection = wc,
	basicGates = bg,
	circuitState = cs } = case M.lookup iw wc of
		Just ow -> case M.lookup ow bg of
			Just g -> second ((ow ,) <$>) $ calcGateResult cs g
			Nothing -> error "Oops!"
		Nothing -> (fromJust $ M.lookup iw cs, Nothing)

data Bit = O | I deriving (Show, Eq)

flipBit :: Bit -> Bit
flipBit O = I
flipBit I = O

andBit, orBit :: Bit -> Bit -> Bit
andBit I I = I
andBit _ _ = O

orBit O O = O
orBit _ _ = I

data Circuit = Circuit {
	wireConnection :: Map InWire OutWire,
	basicGates :: Map OutWire BasicGate,
	circuitState :: Map InWire Bit
	} deriving Show

getOutWire :: OutWire -> Circuit -> Bit
getOutWire ow Circuit {
	basicGates = bgs,
	circuitState = cs } = case M.lookup ow bgs of
	Just g -> fst $ calcGateResult cs g
	Nothing -> error "Oops!"

data BasicGate
	= AndGate InWire InWire | OrGate InWire InWire | NotGate InWire
	| Delay [Bit] InWire
	deriving Show

getGateWires :: BasicGate -> [InWire]
getGateWires (AndGate iw1 iw2) = [iw1, iw2]
getGateWires (OrGate iw1 iw2) = [iw1, iw2]
getGateWires (NotGate iw) = [iw]
getGateWires (Delay _ iw) = [iw]

newtype InWire = InWire Word32 deriving (Show, Eq, Ord)
newtype OutWire = OutWire Word32 deriving (Show, Eq, Ord)

type CreateCircuit = State CreateCircuitStt

data CreateCircuitStt = CreateCircuitStt {
	wireNum :: Word32,
	wireConnectionStt :: Map InWire OutWire,
	basicGatesStt :: Map OutWire BasicGate
	} deriving Show

initialCreateCircuitStt :: CreateCircuitStt
initialCreateCircuitStt = CreateCircuitStt {
	wireNum = 0,
	wireConnectionStt = M.empty,
	basicGatesStt = M.empty }

type CreateCircuitSttMod = CreateCircuitStt -> CreateCircuitStt

succWireNum :: CreateCircuitSttMod
succWireNum ccs = ccs { wireNum = wireNum ccs + 1 }

addWireConnection :: OutWire -> InWire -> CreateCircuitSttMod
addWireConnection ow iw css =
	css { wireConnectionStt = M.insert iw ow $ wireConnectionStt css }

addBasicGate :: BasicGate -> OutWire -> CreateCircuitSttMod
addBasicGate bg ow css =
	css { basicGatesStt = M.insert ow bg $ basicGatesStt css }

createInWire :: CreateCircuit InWire
createInWire = InWire <$> getModify wireNum succWireNum

createOutWire :: CreateCircuit OutWire
createOutWire = OutWire <$> getModify wireNum succWireNum

connectWire :: OutWire -> InWire -> CreateCircuit ()
connectWire ow iw = modify $ addWireConnection ow iw

createAndGate, createOrGate :: CreateCircuit (InWire, InWire, OutWire)
createAndGate = do
	(iw1, iw2, ow) <-
		(,,) <$> createInWire <*> createInWire <*> createOutWire
	modify $ addBasicGate (AndGate iw1 iw2) ow
	return (iw1, iw2, ow)

createOrGate = do
	(iw1, iw2, ow) <-
		(,,) <$> createInWire <*> createInWire <*> createOutWire
	modify $ addBasicGate (OrGate iw1 iw2) ow
	return (iw1, iw2, ow)

createNotGate :: CreateCircuit (InWire, OutWire)
createNotGate = do
	iw <- createInWire
	ow <- createOutWire
	modify $ addBasicGate (NotGate iw) ow
	return (iw, ow)

createDelay :: Word8 -> CreateCircuit (InWire, OutWire)
createDelay w = do
	iw <- createInWire
	ow <- createOutWire
	modify $ addBasicGate (Delay (genericReplicate w O) iw) ow
	return (iw, ow)

andGate :: CreateCircuit (InWire, InWire, OutWire)
andGate = do
	(iw1, iw2, aow) <- createAndGate
	(diw, dow) <- createDelay 10
	connectWire aow diw
	return (iw1, iw2, dow)

orGate :: CreateCircuit (InWire, InWire, OutWire)
orGate = do
	(iw1, iw2, oow) <- createOrGate
	(diw, dow) <- createDelay 10
	connectWire oow diw
	return (iw1, iw2, dow)

notGate :: CreateCircuit (InWire, OutWire)
notGate = do
	(iw, ow) <- createNotGate
	(diw, dow) <- createDelay 5
	connectWire ow diw
	return (iw, dow)

idGate :: CreateCircuit (InWire, OutWire)
idGate = createDelay 0

--------------------------------------------------------------------------------

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m
