{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit (
	Circuit(..), makeCircuit, connect, andGate, orGate, notGate ) where

import Control.Arrow
import Control.Monad.State
import Data.Map.Strict

import Tools

newtype IWire = IWire Word deriving (Show, Eq, Ord)
newtype OWire = OWire Word deriving (Show, Eq, Ord)

data Circuit = Circuit {
	circuitGates :: Map OWire BasicGate,
	circuitConnections :: Map IWire OWire }
	deriving Show

initCircuit :: Circuit
initCircuit = Circuit { circuitGates = empty, circuitConnections = empty }

data BasicGate
	= AndGate IWire IWire | OrGate IWire IWire | NotGate IWire deriving Show

type CircuitBuilder = State (Word, Circuit)

initCircuitBuilderState :: (Word, Circuit)
initCircuitBuilderState = (0, initCircuit)

makeCircuit :: CircuitBuilder a -> (a, Circuit)
makeCircuit = second snd . (`runState` initCircuitBuilderState)

makeIWire :: CircuitBuilder IWire
makeIWire = IWire <$> getModify fst (first (+ 1))

makeOWire :: CircuitBuilder OWire
makeOWire = OWire <$> getModify fst (first (+ 1))

connect :: OWire -> IWire -> CircuitBuilder ()
connect o i = modify $ second ins
	where ins cct = cct {
		circuitConnections = insert i o $ circuitConnections cct }

andGate, orGate :: CircuitBuilder (IWire, IWire, OWire)
andGate = do
	abo@(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify . second $ insGate (AndGate a b) o
	return abo

orGate = do
	abo@(a, b, o) <- (,,) <$> makeIWire <*> makeIWire <*> makeOWire
	modify . second $ insGate (OrGate a b) o
	return abo

notGate :: CircuitBuilder (IWire, OWire)
notGate = do
	io@(i, o) <- (,) <$> makeIWire <*> makeOWire
	modify . second $ insGate (NotGate i) o
	return io

insGate :: BasicGate -> OWire -> Circuit -> Circuit
insGate g o cct = cct { circuitGates = insert o g $ circuitGates cct }
