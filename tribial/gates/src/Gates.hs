{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gates (
	CreateCircuit, createCircuit, circuitState,
	Bit(..),
	connectWire,
	andGate, orGate, notGate,
	setBit, nextWire, nextCircuit, getOutWire,
	mux2 ) where
--	mux2, getMux2Wires, setMux2Wires ) where

import Control.Arrow
import Control.Monad.State
import Data.Bool
import Data.Maybe
import Data.List
import Data.Word
import qualified Data.Set as S

-- import DictList
import DictMap

createCircuit :: CreateCircuit a -> (a, Circuit)
createCircuit cc = let
	(x, CreateCircuitStt {
		wireConnectionStt = wc,
		basicGatesStt = bg
		}) = cc `runState` initialCreateCircuitStt in
	(x, Circuit {
		wireConnection = wc,
		basicGates = bg,
		circuitState = fromListDict $ zip
			(concatMap getGateWires $ elemsDict bg)
			(repeat O) })

setBit :: InWire -> Bit -> Circuit -> Circuit
setBit w b c = c { circuitState = insertDict w b $ circuitState c }

nextCircuit :: Circuit -> Circuit
nextCircuit cc@Circuit {
	circuitState = cs, basicGates = bg } = let
	(ds, ncs) = updateAndAccum (`nextWire` cc) [] cs in
	cc {	circuitState = ncs,
		basicGates = foldr (uncurry insertDict) bg ds }

nextCircuit2 :: Circuit -> Circuit
nextCircuit2 cct@Circuit {
	circuitState = cs, basicGates = bg } = let
	(ows, ds) = nextOutWires (keysDict bg) cct
	ncs = map (flip (nextInWire ows) cct) $ keysDict cs in
	cct {	circuitState = fromListDict $ zip (keysDict cs) ncs,
		basicGates = foldr (uncurry insertDict) bg ds }

calcGateResult :: Dict InWire Bit -> BasicGate -> (Bit, Maybe BasicGate)
calcGateResult cs (AndGate iw1 iw2) =
	(andBit (fromJust $ lookupDict iw1 cs) (fromJust $ lookupDict iw2 cs), Nothing)
calcGateResult cs (OrGate iw1 iw2) =
	(orBit (fromJust $ lookupDict iw1 cs) (fromJust $ lookupDict iw2 cs), Nothing)
calcGateResult cs (NotGate niw) =
	(flipBit . fromJust $ lookupDict niw cs, Nothing)
calcGateResult cs (Delay [] iw) = (fromJust $ lookupDict iw cs, Nothing)
calcGateResult cs (Delay (b : bs) iw) =
	(b, Just $ Delay (bs ++ [fromJust $ lookupDict iw cs]) iw)

-- nextWire :: OutWire -> Circuit -> ...

nextOutWires ::
	[OutWire] -> Circuit -> (Dict OutWire Bit, [(OutWire, BasicGate)])
nextOutWires ows cs = (foldr (uncurry insertDict) emptyDict *** catMaybes) $ unzip
	$ map (\(a, (b, c)) -> ((a, b), (a ,) <$> c))
		$ zip ows $ map (`nextOutWire` cs) ows

nextOutWire :: OutWire -> Circuit -> (Bit, Maybe BasicGate)
nextOutWire ow Circuit { basicGates = bg, circuitState = cs } =
	case lookupDict ow bg of
		Just g -> calcGateResult cs g
		Nothing -> error "Oops!"

nextInWire :: Dict OutWire Bit -> InWire -> Circuit -> Bit
nextInWire ows iw Circuit {
	wireConnection = wc, circuitState = cs } = case lookupDict iw wc of
	Just ow -> fromJust $ lookupDict ow ows
	Nothing -> fromJust $ lookupDict iw cs

nextWire :: InWire -> Circuit -> (Bit, Maybe (OutWire, BasicGate))
nextWire iw Circuit {
	wireConnection = wc,
	basicGates = bg,
	circuitState = cs } = case lookupDict iw wc of
		Just ow -> case lookupDict ow bg of
			Just g -> second ((ow ,) <$>) $ calcGateResult cs g
			Nothing -> error "Oops!"
		Nothing -> (fromJust $ lookupDict iw cs, Nothing)

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
	wireConnection :: Dict InWire OutWire,
	basicGates :: Dict OutWire BasicGate,
	circuitState :: Dict InWire Bit
	} deriving Show

getOutWire :: OutWire -> Circuit -> Bit
getOutWire ow Circuit {
	basicGates = bgs,
	circuitState = cs } = case lookupDict ow bgs of
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

data InWire = InWire Word32 deriving (Show, Eq, Ord)
data OutWire = OutWire Word32 deriving (Show, Eq, Ord)

type CreateCircuit = State CreateCircuitStt

data CreateCircuitStt = CreateCircuitStt {
	wireNum :: Word32,
	wireConnectionStt :: Dict InWire OutWire,
	basicGatesStt :: Dict OutWire BasicGate
	} deriving Show

initialCreateCircuitStt :: CreateCircuitStt
initialCreateCircuitStt = CreateCircuitStt {
	wireNum = 0,
	wireConnectionStt = emptyDict,
	basicGatesStt = emptyDict }

type CreateCircuitSttMod = CreateCircuitStt -> CreateCircuitStt

succWireNum :: CreateCircuitSttMod
succWireNum ccs = ccs { wireNum = wireNum ccs + 1 }

addWireConnection :: OutWire -> InWire -> CreateCircuitSttMod
addWireConnection ow iw css =
	css { wireConnectionStt = insertDict iw ow $ wireConnectionStt css }

addBasicGate :: BasicGate -> OutWire -> CreateCircuitSttMod
addBasicGate bg ow css =
	css { basicGatesStt = insertDict ow bg $ basicGatesStt css }

createInWire :: CreateCircuit InWire
createInWire = InWire <$> getModify wireNum succWireNum

createOutWire :: CreateCircuit OutWire
createOutWire = OutWire <$> getModify wireNum succWireNum

connectWire :: OutWire -> InWire -> CreateCircuit ()
connectWire ow iw = modify $ addWireConnection ow iw

createAndGate, createOrGate :: CreateCircuit (InWire, InWire, OutWire)
createAndGate = do
	iw1 <- createInWire
	iw2 <- createInWire
	ow <- createOutWire
	modify $ addBasicGate (AndGate iw1 iw2) ow
	return (iw1, iw2, ow)

createOrGate = do
	iw1 <- createInWire
	iw2 <- createInWire
	ow <- createOutWire
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

some :: CreateCircuit InWire
some = do
	(iw, ow) <- notGate
	return iw

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
	zip ["a", "b", "s"] . elemsDict
--		. filter ((`elem` [a, b, s]) . fst) . circuitState &&&
		. (`restrictKeysDict` S.fromList [a, b, s]) . circuitState &&&
	(("c" ,) . getOutWire c)

setMux2Wires ::
	(InWire, InWire, InWire, a) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setMux2Wires (a, b, s, _) (ba, bb, bs) = setBit a ba . setBit b bb . setBit s bs

--------------------------------------------------------------------------------

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

--------------------------------------------------------------------------------
