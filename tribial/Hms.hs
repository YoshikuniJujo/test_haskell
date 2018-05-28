{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Hms (HM, sconcat) where

import Data.Semigroup
import Data.String

data HM = HM Int Int

instance Show HM where
	show (HM h m) = show h ++ ":" ++ replicate (2 - length m') '0' ++ m'
		where m' = show m

instance IsString HM where
	fromString hm = HM (read h) (read m)
		where (h, ':' : m) = span (/= ':') hm

instance Semigroup HM where
	HM h1 m1 <> HM h2 m2 =
		HM (h1 + h2 + (m1 + m2) `div` 60) ((m1 + m2) `mod` 60)

instance Num HM where
	(+) = (<>)
	(*) = undefined
	negate (HM h m) = HM (- h) (- m)
	abs (HM h m) = HM (abs h) (abs m)
	signum = undefined
	fromInteger n = HM (fromInteger $ n `div` 60) (fromInteger $ n `mod` 60)
