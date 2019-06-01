{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MapCircuit where

import Data.Map.Strict

import CircuitTypes

newtype GateMap = GateMap (Map OWire BasicGate) deriving Show

newtype WireConnMap = WireConnMap (Map IWire OWire) deriving Show
