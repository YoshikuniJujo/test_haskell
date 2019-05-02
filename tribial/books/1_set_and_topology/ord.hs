{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

newtype Mod = Mod Integer deriving (Show, Eq)

instance Ord Mod where
	Mod m <= Mod n = n `mod` m == 0
