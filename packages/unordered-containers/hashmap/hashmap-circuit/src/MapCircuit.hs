module MapCircuit where

import Data.Map.Strict

import Circuit
import CircuitTypes

type MapCircuit = Circuit (Map OWire) (Map IWire) (Map IWire)
