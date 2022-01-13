{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Curry where

import Language.Haskell.TH

crr2Fun :: Q Dec
crr2Fun = newName "f" >>= \f -> newName "x" >>= \x -> newName "y" >>= \y ->
	funD (mkName "crr2") [
		clause [varP f, varP x, varP y]
			(normalB $ varE f `appE` tupE [	varE x, varE y])
			[] ]
