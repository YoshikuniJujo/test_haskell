{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Language.Haskell.TH

import TribialTools
import CheckEndian

do
	runIO do
		putStrLn $ mkTitle "Hello, template world!"
		print =<< targetEndian
	pure []
