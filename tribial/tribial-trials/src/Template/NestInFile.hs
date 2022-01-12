{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.NestInFile where

import Language.Haskell.TH

foo = $(litE $ integerL 123)

bar = integerL 321

-- baz = $(litE bar)
