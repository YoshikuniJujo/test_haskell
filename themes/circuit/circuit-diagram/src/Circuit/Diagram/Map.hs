{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Circuit.Diagram.Map where

import Data.Map.Strict

data DiagramMap = DiagramMap {
	width :: Word,
	height :: Int,
	layout :: Map (Word, Int) Element }
	deriving Show

data Element
	= Stump
	| AndGateE | OrGateE | NotGateE
	| HLine | VLine
	| TopLeft | TopRight | BottomLeft | BottomRight
	| TShape | TInverted | TLeft | TRight | CrossDot | Cross
	deriving Show
