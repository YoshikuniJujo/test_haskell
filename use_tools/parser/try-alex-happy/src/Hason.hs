{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hason (Hason, HasonKey(..), HasonValue(..)) where

type Hason = [(HasonKey, HasonValue)]

data HasonKey = KInt Integer | KStr String deriving Show

data HasonValue
	= Int Integer | Str String
	| Seq [HasonValue] | Dct Hason
	deriving Show
