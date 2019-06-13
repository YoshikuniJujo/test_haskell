{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CircuitCore (
	Circuit, makeCircuit, step,
	CircuitBuilder,
	IWire, OWire, Bits(..), BitLen, BitPosIn, BitPosOut,
	andGate, orGate, notGate, idGate, constGate, triStateSelect,
	connectWire, delay,
	setBits, peekOWire, bitsToWord, wordToBits
	) where

import Prelude
import qualified Prelude as P

import Control.Monad.State
import Data.Maybe
import Data.Map.Strict
import Data.Word

import CircuitTypes
import Tools

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit cb = (x ,) $ Circuit {
	cctGate = gs, cctWireConn = wc,
	cctWireStt = fromList $ makeWireState dm <$> (gateWires =<< concat (elems gs)) }
--		$ zip (gateWires =<< concat (elems gs)) (repeat $ replicate 1 (Bits 0)) }
	where
	(x, CBState { cbsGate = gs, cbsWireConn = wc, cbsDelay = dm }) =
		cb `runState` initCBState

makeWireState :: Map IWire Word8 -> IWire -> (IWire, [Bits])
makeWireState dm iw = (iw, replicate d $ Bits 0)
	where d = fromMaybe 1 $ fromIntegral <$> dm !? iw

step :: Circuit -> Circuit
step cct@Circuit { cctGate = gs, cctWireStt = wst } = let
	ows = M.map (checkOWire cct) gs in
	cct { cctWireStt = mapWithKey (nextIWire cct ows) wst }

setBits :: IWire -> Bits -> Circuit -> Circuit
setBits w bs c = c { cctWireStt = insertPush w bs $ cctWireStt c }

insertPush :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertPush k v m = insert k (init vs ++ [v]) m
	where vs = fromMaybe [] $ m !? k

peekOWire :: OWire -> Circuit -> Bits
peekOWire w Circuit { cctGate = gs, cctWireStt = wst } =
	fromJustMsg "peekOWire" $ P.foldr (calcGate wst) (Bits 0) <$> gs !? w

checkOWire :: Circuit -> [BasicGate] -> Bits
checkOWire Circuit { cctWireStt = wst } = P.foldr (calcGate wst) (Bits 0)

calcGate :: Map IWire [Bits] -> BasicGate -> Bits -> Bits
calcGate wst (AndGate ln po (i1, pi1) (i2, pi2)) = fromJust
	$ andBits ln po
		<$> ((, pi1) <$> wst !!? i1) <*> ((, pi2) <$> wst !!? i2)
calcGate wst (OrGate ln po (i1, pi1) (i2, pi2)) = fromJust
	$ orBits ln po <$> ((, pi1) <$> wst !!? i1) <*> ((, pi2) <$> wst !!? i2)
calcGate wst (NotGate ln po (i, pin)) =
	fromJust $ notBits ln po <$> ((, pin) <$> wst !!? i)
calcGate wst (IdGate ln po (i, pin)) =
	fromJust $ idBits ln po <$> ((, pin) <$> wst !!? i)
calcGate _ (ConstGate ln po (bs, pin)) = constBits ln po (bs, pin)
calcGate wst (TriStateSelect sel is) = maybe id const $ do
--	const . fromJustMsg "calcGate: TriStateSelect: " $ do
		s <- wst !!? sel
		i <- is IM.!? fromIntegral (bitsToWord s)
		wst !!? i

fromJustMsg :: String -> Maybe a -> a
fromJustMsg msg Nothing = error $ "fromJustMsg: " ++ msg
fromJustMsg _ (Just x) = x

(!!?) :: Ord k => Map k [v] -> k -> Maybe v 
m !!? k = join $ listToMaybe <$> m !? k

nextIWire :: Circuit -> Map OWire Bits -> IWire -> [Bits] -> [Bits]
nextIWire Circuit { cctWireConn = wc } ows iw oba@(_ : obs) =
	(obs ++) . (: []) . fromMaybe ob $ do
		fows <- wc !? iw
		return $ P.foldr
			(uncurry . flip $ nextIWireFromOWire ows) ob fows
	where
	ob = last oba
nextIWire _ _ _ [] = error "Oops!"

nextIWireFromOWire :: Map OWire Bits -> FromOWire -> OWire -> Bits -> Bits
nextIWireFromOWire ows fow ow b = fromMaybe b $ do
	owb <- ows !? ow
	return $ fromOWire fow owb b

andGate, orGate :: BitLen -> BitPosIn -> BitPosIn -> BitPosOut ->
	CircuitBuilder (IWire, IWire, OWire)
andGate ln pi1 pi2 po = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (AndGate ln po (a, pi1) (b, pi2)) o
	return (a, b, o)

orGate ln pi1 pi2 po = do
	(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify $ insGate (OrGate ln po (a, pi1) (b, pi2)) o
	return (a, b, o)

notGate :: BitLen -> BitPosIn -> BitPosOut -> CircuitBuilder (IWire, OWire)
notGate ln pin po = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (NotGate ln po (i, pin)) o
	return (i, o)

idGate :: BitLen -> BitPosIn -> BitPosOut -> CircuitBuilder (IWire, OWire)
idGate ln pin po = do
	(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify $ insGate (IdGate ln po (i, pin)) o
	return (i, o)

constGate :: Bits -> BitLen -> BitPosIn -> BitPosOut -> CircuitBuilder OWire
constGate bs ln pin po = do
	o <- makeOWire
	modify $ insGate (ConstGate ln po (bs, pin)) o
	return o

triStateSelect :: Word8 -> CircuitBuilder (IWire, [IWire], OWire)
triStateSelect n = do
	sel <- makeIWire
	is <- fromIntegral n `replicateM` makeIWire
	o <- makeOWire
	modify $ insGate (TriStateSelect sel (IM.fromList $ zip [0 ..] is)) o
	return (sel, is, o)

insGate :: BasicGate -> OWire -> CBState -> CBState
insGate g o cbs = cbs { cbsGate = push o g $ cbsGate cbs }

connectWire :: (OWire, BitLen, BitPosOut) ->
	(IWire, BitLen, BitPosIn) -> CircuitBuilder ()
connectWire (o, obl, obp) (i, ibl, ibp) =
	modify $ insConn ((obl, obp), (ibl, ibp)) o i

delay :: IWire -> Word8 -> CircuitBuilder ()
delay _ 0 = error "0 delay is not permitted"
delay iw n = modify $ insDelay iw n

insConn :: FromOWire -> OWire -> IWire -> CBState -> CBState
insConn f o i cbs = cbs {
	cbsWireConn =
		M.insert i ((o, f) : indexOrEmpty (cbsWireConn cbs) i)
			$ cbsWireConn cbs }

indexOrEmpty :: Ord k => Map k [v] -> k -> [v]
indexOrEmpty = (fromMaybe [] .) . (!?)

insDelay :: IWire -> Word8 -> CBState -> CBState
insDelay iw d cbs = cbs { cbsDelay = M.insert iw d $ cbsDelay cbs }
