{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Idou where

import Prelude hiding (id, (.))

import Control.Category

data Izakaya = Izakaya deriving Show
data Konbini = Konbini deriving Show
data Jitaku = Jitaku deriving Show

data Idou a b = Idou Int deriving Show

instance Category Idou where
	id = Idou 1
	Idou a . Idou b = Idou $ a * b

izakaya :: Idou Izakaya Izakaya
izakaya = Idou 0

konbini :: Idou Konbini Konbini
konbini = Idou 0

jitaku :: Idou Jitaku Jitaku
jitaku = Idou 0

izakayaKonbini1, izakayaKonbini2 :: Idou Izakaya Konbini
izakayaKonbini1 = Idou (2 ^ 1)
izakayaKonbini2 = Idou (2 ^ 2)

konbiniJitaku1, konbiniJitaku2, konbiniJitaku3 :: Idou Konbini Jitaku
konbiniJitaku1 = Idou (3 ^ 1)
konbiniJitaku2 = Idou (3 ^ 2)
konbiniJitaku3 = Idou (3 ^ 3)
