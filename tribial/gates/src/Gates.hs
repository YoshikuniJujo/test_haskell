{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gates (
	CreateCircuit, createCircuit, circuitState,
	Bit(..),
	connectWire,
	andGateD, orGateD, notGateD,
	setBit, nextWire, nextCircuit, getOutWire,
	example, mux2, getMux2Wires, setMux2Wires ) where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Word

createCircuit :: CreateCircuit a -> (a, Circuit)
createCircuit cc = let
	(x, CreateCircuitStt {
		wireConnectionStt = wc,
		basicGatesStt = bg
		}) = cc `runState` initialCreateCircuitStt in
	(x, Circuit {
		wireConnection = wc,
		basicGates = bg,
		circuitState = zip
			(concatMap (getGateWires . snd) bg)
			(repeat O) })

setBit :: InWire -> Bit -> Circuit -> Circuit
setBit w b c = c { circuitState = (w, b) : circuitState c }

nextCircuit :: Circuit -> Circuit
nextCircuit cc@Circuit {
	circuitState = cs, basicGates = bg } = let
	(ncs, ds) = unzip $ map (flip nextWire cc . fst) cs in
	cc {	circuitState = zip (map fst cs) ncs,
		basicGates = catMaybes ds ++ bg }
--	cc { circuitState = zip (map fst cs) $ map (flip nextWire cc . fst) cs }

calcGateResult :: [(InWire, Bit)] -> BasicGate -> (Bit, Maybe BasicGate)
calcGateResult cs (AndGate iw1 iw2) =
	(andBit (fromJust $ lookup iw1 cs) (fromJust $ lookup iw2 cs), Nothing)
calcGateResult cs (OrGate iw1 iw2) =
	(orBit (fromJust $ lookup iw1 cs) (fromJust $ lookup iw2 cs), Nothing)
calcGateResult cs (NotGate niw) =
	(flipBit . fromJust $ lookup niw cs, Nothing)
calcGateResult cs (Delay [] iw) = (fromJust $ lookup iw cs, Nothing)
calcGateResult cs (Delay (b : bs) iw) =
	(b, Just $ Delay (bs ++ [fromJust $ lookup iw cs]) iw)

nextWire :: InWire -> Circuit -> (Bit, Maybe (OutWire, BasicGate))
nextWire iw (Circuit {
	wireConnection = wc,
	basicGates = bg,
	circuitState = cs }) = case lookup iw wc of
		Just ow -> case lookup ow bg of
			Just g -> second ((ow ,) <$>) $ calcGateResult cs g
			Nothing -> error "Oops!"
		Nothing -> (fromJust $ lookup iw cs, Nothing)

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
	wireConnection :: [(InWire, OutWire)],
	basicGates :: [(OutWire, BasicGate)],
	circuitState :: [(InWire, Bit)]
	} deriving Show

getOutWire :: OutWire -> Circuit -> Bit
getOutWire ow Circuit {
	basicGates = bgs,
	circuitState = cs } = case lookup ow bgs of
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

data InWire = InWire Word32 deriving (Show, Eq)
data OutWire = OutWire Word32 deriving (Show, Eq)

type CreateCircuit = State CreateCircuitStt

data CreateCircuitStt = CreateCircuitStt {
	wireNum :: Word32,
	wireConnectionStt :: [(InWire, OutWire)],
	basicGatesStt :: [(OutWire, BasicGate)]
	} deriving Show

initialCreateCircuitStt :: CreateCircuitStt
initialCreateCircuitStt = CreateCircuitStt {
	wireNum = 0,
	wireConnectionStt = [],
	basicGatesStt = [] }

type CreateCircuitSttMod = CreateCircuitStt -> CreateCircuitStt

succWireNum :: CreateCircuitSttMod
succWireNum ccs = ccs { wireNum = wireNum ccs + 1 }

addWireConnection :: OutWire -> InWire -> CreateCircuitSttMod
addWireConnection ow iw css =
	css { wireConnectionStt = (iw, ow) : wireConnectionStt css }

addBasicGate :: BasicGate -> OutWire -> CreateCircuitSttMod
addBasicGate bg ow css = css { basicGatesStt = (ow, bg) : basicGatesStt css }

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

andGateD :: CreateCircuit (InWire, InWire, OutWire)
andGateD = do
	(iw1, iw2, aow) <- createAndGate
	(diw, dow) <- createDelay 10
	connectWire aow diw
	return (iw1, iw2, dow)

orGateD :: CreateCircuit (InWire, InWire, OutWire)
orGateD = do
	(iw1, iw2, oow) <- createOrGate
	(diw, dow) <- createDelay 10
	connectWire oow diw
	return (iw1, iw2, dow)

notGateD :: CreateCircuit (InWire, OutWire)
notGateD = do
	(iw, ow) <- createNotGate
	(diw, dow) <- createDelay 5
	connectWire ow diw
	return (iw, dow)

createIdGate :: CreateCircuit (InWire, OutWire)
createIdGate = createDelay 0

example :: CreateCircuit InWire
example = do
	(niw, now) <- createNotGate
	(diw, dow) <- createDelay 10
	connectWire now diw
	connectWire dow niw
	return niw

--------------------------------------------------------------------------------

mux2 :: CreateCircuit (InWire, InWire, InWire, OutWire)
mux2 = do
	(a, s1, a1o) <- createAndGate
	(b, s2, a2o) <- createAndGate
	(xi1, xi2, c) <- createOrGate
	(ni, no) <- createNotGate
	(si, so) <- createIdGate
	connectWire so ni
	connectWire no s1
	connectWire so s2
	connectWire a1o xi1
	connectWire a2o xi2
	return (a, b, si, c)

getMux2Wires :: (InWire, InWire, InWire, OutWire) ->
	Circuit -> ([(String, Bit)], (String, Bit))
getMux2Wires (a, b, s, c) =
	zip ["a", "b", "s"] . map snd
		. filter ((`elem` [a, b, s]) . fst) . circuitState &&&
	(("c" ,) . getOutWire c)

setMux2Wires ::
	(InWire, InWire, InWire, a) -> (Bit, Bit, Bit) -> Circuit -> Circuit
setMux2Wires (a, b, s, _) (ba, bb, bs) = setBit a ba . setBit b bb . setBit s bs

--------------------------------------------------------------------------------

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m
