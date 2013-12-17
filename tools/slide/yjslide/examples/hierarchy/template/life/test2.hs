{-# LANGUAGE TemplateHaskell, ExistentialQuantification, DeriveDataTypeable #-}

import Data.Typeable

import LifeTemplate
import Template

mkTop "Show" "Hoge"
mkTopShow "Hoge"

topType "Hoge" "Hagenai"
topTypeShow "Hagenai"
addType "Hoge" "Hagenai" "FusaFusa"
addTypeShow "FusaFusa"

castHoge :: (Hoge l, Hoge m) => l -> Maybe m
castHoge = fromHoge . toHoge
