{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TemplateRandom where

import Language.Haskell.TH
import System.Random

foo, bar :: Integer
foo = $(litE $ integerL 123)
bar = $(litE . integerL . abs =<< runIO randomIO)

type Foo = 123
type Bar = $(litT . numTyLit . abs =<< runIO randomIO)
