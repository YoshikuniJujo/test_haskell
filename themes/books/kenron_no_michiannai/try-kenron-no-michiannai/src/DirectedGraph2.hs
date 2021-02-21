{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DirectedGraph2 where

data DiGraph e v = DiGraph { start :: e -> v, end :: e -> v }

data Vertex = A | B | C | D deriving Show
data Edge = Ea | Eb | Ec deriving Show

sample :: DiGraph Edge Vertex
sample = DiGraph {
	start = \case Ea -> A; Eb -> B; Ec -> C,
	end = \case Ea -> B; Eb -> C; Ec -> D }

type DiGraph' e v = (e -> v, e -> v)

sample' :: DiGraph' Edge Vertex
sample' = (
	\case Ea -> A; Eb -> B; Ec -> C,
	\case Ea -> B; Eb -> C; Ec -> D )
