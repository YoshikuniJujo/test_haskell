{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Blackhole where

data Blackhole = forall x . Blackhole x

data ShowYou = forall s . Show s => ShowYou s

instance Show ShowYou where
	show (ShowYou s) = "(ShowYou " ++ show s ++ ")"
