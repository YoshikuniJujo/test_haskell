{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable

import LifeTemplate
import Template

mkTop "Show" "Life"
mkTopShow "Life"

newType mkSomeDStr "Life" "Life" "Animal"
sequence [
	instanceShow "Animal",
	instanceShow "SomeAnimal" ]

newType mkSomeDStr "Life" "Animal" "Human"
sequence [
	instanceShow "Human",
	instanceShow "SomeHuman" ]

castLife :: (Life l, Life m) => l -> Maybe m
castLife = fromLife . toLife
