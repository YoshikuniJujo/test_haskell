{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH

moduleName :: String
moduleName = $(litE =<< stringL . loc_module <$> location)

sampleError :: String
sampleError = $(do
--	reportError "foobar"
	reportWarning "foobar"
	litE $ stringL "foobar")

loc :: String
loc = $(litE =<< stringL . pprint <$> location)
