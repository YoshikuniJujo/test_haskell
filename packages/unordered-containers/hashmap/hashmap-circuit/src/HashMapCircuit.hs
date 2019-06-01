module HashMapCircuit where

import Data.HashMap.Strict

import Circuit
import CircuitTypes

type HashMapCircuit = Circuit (HashMap OWire) (HashMap IWire) (HashMap IWire)
