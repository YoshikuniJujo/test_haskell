{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit, CircuitBuilder, makeCircuit, step, setBit, peekOWire,
	IWire, OWire, Bit(..), andGate, orGate, notGate, idGate, triGate, delay, connectWire,
	makeInnerCircuit, makeInnerCircuit2, makeInnerCircuitList
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
calcGate wst (InnerCircuit i cct ii io) = do
	b <- wst !? i
	let	cct' = step $ setBit ii b cct
	return (Just $ InnerCircuit i cct' ii io, peekOWire io cct')
calcGate wst (InnerCircuit2 i1 i2 cct ii1 ii2 io) = do
	b1 <- wst !? i1
	b2 <- wst !? i2
	let	cct' = step . setBit ii1 b1 $ setBit ii2 b2 cct
	return (Just $ InnerCircuit2 i1 i2 cct' ii2 ii2 io, peekOWire io cct')
calcGate wst (InnerCircuitList is i ccts) = do
	bs <- mapM (wst !?) is
	b <- wst !? i
	let	ccts' = map (flip setBitsIoWire b) ccts
		mo = do	n <- bitsToInt bs
			let	(cct, _ii, io) = ccts' !! n
			return $ peekOWire io cct
		o = fromMaybe X mo
	return (Just $ InnerCircuitList is i ccts', o)

setBitsIoWire :: (Circuit, IWire, OWire) -> Bit -> (Circuit, IWire, OWire)
setBitsIoWire (cct, i, o) b = (step $ setBit i b cct, i, o)

bitsToInt :: [Bit] -> Maybe Int
bitsToInt [] = Just 0
bitsToInt (O : bs) = (* 2) <$> bitsToInt bs
bitsToInt (I : bs) = (+ 1) . (* 2) <$> bitsToInt bs
bitsToInt _ = Nothing

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

-- newtype LazyGateId = LazyGateId Word32

data BasicGate
	= AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| TriGate IWire IWire | Delay [Bit] IWire
	| InnerCircuit IWire Circuit IWire OWire
	| InnerCircuit2 IWire IWire Circuit IWire IWire OWire
	| InnerCircuitList [IWire] IWire [(Circuit, IWire, OWire)]
	{-
	| LazyGate [IWire]
		(Map Word32 Circuit)
		(LazyGateId -> Word32 -> ([IWire], Circuit, OWire))
		-}
	deriving Show

{-
instance Show BasicGate where
	show _ = "<BasicGate>"
	-}

gateWires :: BasicGate -> [IWire]
gateWires (AndGate i1 i2) = [i1, i2]
gateWires (OrGate i1 i2) = [i1, i2]
gateWires (NotGate i) = [i]
gateWires (TriGate i1 i2) = [i1, i2]
gateWires (Delay _ i) = [i]
gateWires (InnerCircuit i _ _ _) = [i]
gateWires (InnerCircuit2 i1 i2 _ _ _ _) = [i1, i2]
gateWires (InnerCircuitList is i _) = is ++ [i]

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

makeInnerCircuit :: Circuit -> IWire -> OWire -> CircuitBuilder (IWire, OWire)
makeInnerCircuit cct ii io = do
	i <- makeIWire
	o <- makeOWire
	modify $ insGate (InnerCircuit i cct ii io) o
	return (i, o)

makeInnerCircuit2 :: Circuit -> IWire -> IWire -> OWire -> CircuitBuilder (IWire, IWire, OWire)
makeInnerCircuit2 cct ii1 ii2 io = do
	i1 <- makeIWire
	i2 <- makeIWire
	o <- makeOWire
	modify $ insGate (InnerCircuit2 i1 i2 cct ii1 ii2 io) o
	return (i1, i2, o)

makeInnerCircuitList :: Word8 -> [(Circuit, IWire, OWire)] -> CircuitBuilder ([IWire], IWire, OWire)
makeInnerCircuitList n ccts = do
	is <- fromIntegral n `replicateM` makeIWire
	i <- makeIWire
	o <- makeOWire
	modify $ insGate (InnerCircuitList is i ccts) o
	return (is, i, o)

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
