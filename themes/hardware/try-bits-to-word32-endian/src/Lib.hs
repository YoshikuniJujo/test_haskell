{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH

import TribialTools

do
	runIO . putStrLn $ mkTitle "Hello, template world!"
	pure []
