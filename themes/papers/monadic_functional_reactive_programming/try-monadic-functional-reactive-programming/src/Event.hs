{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Event (Event(..), Action(..)) where

data Event a = Request | Occurred a deriving Show

instance Ord a => Eq (Event a) where a == b = a `compare` b == EQ

instance Ord a => Ord (Event a) where
	Occurred x `compare` Occurred y = x `compare` y
	Request `compare` _ = EQ
	_ `compare` Request = EQ

data Action a = Response | Cause a deriving Show

instance Ord a => Eq (Action a) where a == b = a `compare` b == EQ

instance Ord a => Ord (Action a) where
	Cause x `compare` Cause y = x `compare` y
	Response `compare` _ = EQ
	_ `compare` Response = EQ
