{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DirectedGraph3 where

data DiGraph = Start | End deriving Show

class DiGraphable e v where
	f :: DiGraph -> (e -> v)

instance DiGraphable Edge Vertex where
	f Start = \case Ea -> A; Eb -> B; Ec -> C
	f End = \case Ea -> B; Eb -> C; Ec -> D

data Vertex = A | B | C | D deriving Show
data Edge = Ea | Eb | Ec deriving Show
