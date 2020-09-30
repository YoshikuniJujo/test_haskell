{-# LANGUAGE TemplateHaskell #-}

module ExampleLifeHierarchy where

import SomeLife
import Language.Haskell.TH

lifeContainer $ mkName "foo"
