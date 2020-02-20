{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event ( Event(..)) where

data Event a = Request | Occurred a deriving Show

instance Ord a => Eq (Event a) where a == b = a `compare` b == EQ

instance Ord a => Ord (Event a) where
	Occurred x `compare` Occurred y = x `compare` y
	Request `compare` _ = EQ
	_ `compare` Request = EQ
