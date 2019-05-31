{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	DoCircuit, Circuit, CircuitBuilder, makeCircuit, step, setBit, setBits, peekOWire,
	IWire, OWire, Bit(..), constGate, andGate, orGate, notGate, idGate, triGate,
	delay, connectWire, lazyGates
	) where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Map.Strict (Map, (!?))
-- import Data.Bool
import Data.Word

import qualified Data.Map.Strict as M

import Tools

type DoCircuit = Circuit -> Circuit

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
calcGate _ (ConstGate b) = Just (Nothing, b)
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
calcGate wst (LazyGate ix is ccts cct0) = do
	bixs <- mapM (wst !?) ix
	bs <- mapM (wst !?) is
	let	mcct1 = flip (M.findWithDefault cct0) ccts . fromIntegral <$> bitsToInt bixs
		mcct1' = (`setBitsIsOWire` bs) <$> mcct1
		ccts' = fromMaybe ccts $ do
			k <- fromIntegral <$> bitsToInt bixs
			v <- (\(x, y, z) -> (step x, y, z)) <$> mcct1'
			return . M.insert k v $ (`setBitsIsOWire` repeat O) <$> ccts
		mo = do	(cct, _ii, io) <- mcct1'
			return $ peekOWire io cct
		o = fromMaybe X mo
	return (Just $ LazyGate ix is ccts' cct0, o)

setBitsIsOWire :: (Circuit, [IWire], OWire) -> [Bit] -> (Circuit, [IWire], OWire)
setBitsIsOWire (cct, is, o) bs = (step $ setBits is bs cct, is, o)

setBits :: [IWire] -> [Bit] -> Circuit -> Circuit
setBits is bs = foldr (.) id $ zipWith setBit is bs

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
	= ConstGate Bit
	| AndGate IWire IWire | OrGate IWire IWire | NotGate IWire
	| TriGate IWire IWire | Delay [Bit] IWire
	| LazyGate [IWire] [IWire]
		(Map Word64 (Circuit, [IWire], OWire)) (Circuit, [IWire], OWire)
	deriving Show

gateWires :: BasicGate -> [IWire]
gateWires (ConstGate _) = []
gateWires (AndGate i1 i2) = [i1, i2]
gateWires (OrGate i1 i2) = [i1, i2]
gateWires (NotGate i) = [i]
gateWires (TriGate i1 i2) = [i1, i2]
gateWires (Delay _ i) = [i]
gateWires (LazyGate ix is _ _) = ix ++ is

connectWire :: OWire -> IWire -> CircuitBuilder ()
connectWire o i = modify $ insConn o i

constGate :: Bit -> CircuitBuilder OWire
constGate b = do
	o <- makeOWire
	modify $ insGate (ConstGate b) o
	return o

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

lazyGates :: Word8 -> CircuitBuilder ([IWire], OWire) ->
	CircuitBuilder ([IWire], [IWire], OWire)
lazyGates n cctb = do
	let	((iis, io), cct0) = makeCircuit cctb
	ixs <- fromIntegral n `replicateM` makeIWire
	is <- length iis `replicateM` makeIWire
	o <- makeOWire
	modify $ insGate (LazyGate ixs is M.empty (cct0, iis, io)) o
	return (ixs, is, o)

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
