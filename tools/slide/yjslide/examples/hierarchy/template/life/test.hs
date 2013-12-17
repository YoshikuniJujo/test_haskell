{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable

import LifeTemplate
import Template

mkTop "Show" "Life"
mkTopShow "Life"

topType "Life" "Animal"
topTypeShow "Animal"
addType "Life" "Animal" "Human"
addTypeShow "Human"

castLife :: (Life l, Life m) => l -> Maybe m
castLife = fromLife . toLife
