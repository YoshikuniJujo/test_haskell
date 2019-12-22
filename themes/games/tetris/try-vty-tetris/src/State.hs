{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State where

data State = State {
	position :: (Int, Int)
	} deriving Show
