{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TemplateTuples where

import Language.Haskell.TH

unit :: $(tupleT 0)
unit = $(tupE [])

singleton :: $(tupleT 1 `appT` conT ''Int)
singleton = $(tupE [litE $ integerL 123])

two :: $(tupleT 2 `appT` conT ''Int `appT` conT ''Int)
two = $(tupE [litE $ integerL 123, litE $ integerL 456])

nakami :: Int
$(tupP [varP $ mkName "nakami"]) = singleton
