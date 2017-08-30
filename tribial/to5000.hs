{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

default(Number)

data Number = One | FiveThousand

instance Num Number where
	_ + _ = FiveThousand
	_ * _ = FiveThousand
	abs = id
	signum _ = One
	_ - _ = FiveThousand
	fromInteger 1 = One
	fromInteger _ = FiveThousand

instance Show Number where
	show One = "1"
	show FiveThousand = "5000"

onePlusOne = 1 + 1
