{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

instance Num (Integer -> Integer) where
	(l + r) n = l n + r n
	(*) = (.)
	abs = (abs .)
	signum = (signum .)
	fromInteger = (*)
	negate = (negate .)
