{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gates (
	CreateCircuit, createCircuit, circuitState,
	Bit(..),
	connectWire,
	andGateD, orGateD, notGateD,
	setBit, nextWire, nextCircuit,
	example ) where

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

nextWire :: InWire -> Circuit -> (Bit, Maybe (OutWire, BasicGate))
nextWire iw (Circuit {
	wireConnection = wc,
	basicGates = bg,
	circuitState = cs }) = case lookup iw wc of
		Just ow -> case lookup ow bg of
			Just (AndGate iw1 iw2) -> let
				b1 = fromJust $ lookup iw1 cs
				b2 = fromJust $ lookup iw2 cs in
				(andBit b1 b2, Nothing)
			Just (OrGate iw1 iw2) -> let
				b1 = fromJust $ lookup iw1 cs
				b2 = fromJust $ lookup iw2 cs in
				(orBit b1 b2, Nothing)
			Just (NotGate niw) ->
				(flipBit . fromJust $ lookup niw cs, Nothing)
			Just (XorGate iw1 iw2) -> let
				b1 = fromJust $ lookup iw1 cs
				b2 = fromJust $ lookup iw2 cs in
				(xorBit b1 b2, Nothing)
			Just (Delay (b : bs) niw) -> let
				nb = fromJust $ lookup niw cs in
				(b, Just (ow, Delay (bs ++ [nb]) niw))
			Just (Delay [] niw) ->
				(fromJust $ lookup niw cs, Nothing)
			Nothing -> error "Oops!"
		Nothing -> (fromJust $ lookup iw cs, Nothing)

data Bit = O | I deriving (Show, Eq)

flipBit :: Bit -> Bit
flipBit O = I
flipBit I = O

andBit, orBit, xorBit :: Bit -> Bit -> Bit
andBit I I = I
andBit _ _ = O

orBit O O = O
orBit _ _ = I

xorBit b1 b2
	| b1 == b2 = O
	| otherwise = I

data Circuit = Circuit {
	wireConnection :: [(InWire, OutWire)],
	basicGates :: [(OutWire, BasicGate)],
	circuitState :: [(InWire, Bit)]
	} deriving Show

data BasicGate
	= AndGate InWire InWire | OrGate InWire InWire | NotGate InWire
	| XorGate InWire InWire
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

createAndGate, createOrGate, createXorGate ::
	CreateCircuit (InWire, InWire, OutWire)
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

createXorGate = do
	iw1 <- createInWire
	iw2 <- createInWire
	ow <- createOutWire
	modify $ addBasicGate (XorGate iw1 iw2) ow
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

xorGateD :: CreateCircuit (InWire, InWire, OutWire)
xorGateD = do
	(iw1, iw2, oow) <- createXorGate
	(diw, dow) <- createDelay 10
	connectWire oow diw
	return (iw1, iw2, dow)

notGateD :: CreateCircuit (InWire, OutWire)
notGateD = do
	(iw, ow) <- createNotGate
	(diw, dow) <- createDelay 5
	connectWire ow diw
	return (iw, dow)

example :: CreateCircuit InWire
example = do
	(niw, now) <- createNotGate
	(diw, dow) <- createDelay 10
	connectWire now diw
	connectWire dow niw
	return niw

{-
mux2 :: CreateCircuit (InWire, InWire, InWire, OutWire)
mux2 = do
	(a1iw1, a1iw2, a1ow) <- createAndGate
	(a2iw1, a2iw2, a2ow) <- createAndGate
	(xiw1, xiw2, xow) <- createXorGate
	connectWire 
	-}

--------------------------------------------------------------------------------

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m
