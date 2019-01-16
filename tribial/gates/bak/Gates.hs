{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gates where

import Control.Monad.State
import Data.Maybe
import Data.Word

type CreateCircuit = State CreateCircuitState

data CreateCircuitState = CreateCircuitState {
	varNumber :: Word32,
	inWireNumber :: Word32,
	outWireNumber :: Word32,
	varEnvIn :: [(Word32, Word32)],
	varEnvOut :: [(Word32, Word32)],
	circuit :: [Either (InWire, OutWire) BasicGate]
	} deriving Show

initialCreateCircuitState :: CreateCircuitState
initialCreateCircuitState = CreateCircuitState {
	varNumber = 0,
	inWireNumber = 0,
	outWireNumber = 0,
	varEnvIn = [],
	varEnvOut = [],
	circuit = [] }

connectWireCreateCircuitState ::
	OutWire -> InWire -> CreateCircuitState -> CreateCircuitState
connectWireCreateCircuitState ow iw cc =
	cc { circuit = Left (iw, ow) : circuit cc }

succVarNumber, succInWireNumber, succOutWireNumber ::
	CreateCircuitState -> CreateCircuitState
succVarNumber cc = cc { varNumber = varNumber cc + 1 }
succInWireNumber cc = cc { inWireNumber = inWireNumber cc + 1 }
succOutWireNumber cc = cc { outWireNumber = outWireNumber cc + 1 }

setVarEnvIn :: Var InWire -> InWire -> CreateCircuitState -> CreateCircuitState
setVarEnvIn (Var v) (InWire w) cc = cc { varEnvIn = (v, w) : varEnvIn cc }

setVarEnvOut ::
	Var OutWire -> OutWire -> CreateCircuitState -> CreateCircuitState
setVarEnvOut (Var v) (OutWire w) cc = cc { varEnvOut = (v, w) : varEnvOut cc }

setBasicGate :: BasicGate -> CreateCircuitState -> CreateCircuitState
setBasicGate bg cc = cc { circuit = Right bg : circuit cc }

newtype Circuit = Circuit [Either (InWire, OutWire) BasicGate]
	deriving Show

data BasicGate
	= AndGate InWire InWire OutWire
	| OrGate InWire InWire OutWire
	| NotGate InWire OutWire
	| Delay Word8 InWire OutWire
	deriving Show

data InWire = InWire Word32 deriving Show

data OutWire = OutWire Word32 deriving Show

data Var a = Var Word32 deriving Show

data Bit = O | I deriving Show

newtype CircuitState = CircuitState [(InWire, Bit)] deriving Show

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = do
	x <- gets g
	modify m
	return x

mkVar :: CreateCircuit (Var a)
mkVar = Var <$> getModify varNumber succVarNumber

lookupVar :: (CreateCircuitState -> [(Word32, Word32)]) ->
	Var a -> CreateCircuit (Maybe Word32)
lookupVar dct (Var v) = gets (lookup v . dct)

lookupVarInMaybe :: Var InWire -> CreateCircuit (Maybe InWire)
lookupVarInMaybe = ((InWire <$>) <$>) . lookupVar varEnvIn

lookupVarOutMaybe :: Var OutWire -> CreateCircuit (Maybe OutWire)
lookupVarOutMaybe = ((OutWire <$>) <$>) . lookupVar varEnvOut

mkInWire :: CreateCircuit InWire
mkInWire = InWire <$> getModify inWireNumber succInWireNumber

createInWire :: CreateCircuit (Var InWire)
createInWire = do
	v <- mkVar
	w <- mkInWire
	modify $ setVarEnvIn v w
	return v

mkOutWire :: CreateCircuit OutWire
mkOutWire = OutWire <$> getModify outWireNumber succOutWireNumber

createOutWire :: CreateCircuit (Var OutWire)
createOutWire = do
	v <- mkVar
	w <- mkOutWire
	modify $ setVarEnvOut v w
	return v

lookupVarIn :: Var InWire -> CreateCircuit InWire
lookupVarIn = (fromJust <$>) . lookupVarInMaybe

lookupVarOut :: Var OutWire -> CreateCircuit OutWire
lookupVarOut = (fromJust <$>) . lookupVarOutMaybe

createNotGate :: CreateCircuit (Var InWire, Var OutWire)
createNotGate = do
	iw <- createInWire
	ow <- createOutWire
	modify . setBasicGate
		=<< (NotGate <$> lookupVarIn iw <*> lookupVarOut ow)
	return (iw, ow)

connectWireGen :: OutWire -> InWire -> CreateCircuit ()
connectWireGen ow iw = modify $ connectWireCreateCircuitState ow iw

connectWire :: Var OutWire -> Var InWire -> CreateCircuit ()
connectWire vow viw = do
	ow <- lookupVarOut vow
	iw <- lookupVarIn viw
	connectWireGen ow iw

-- example :: CreateCircuit (Var InWire)
-- example 
