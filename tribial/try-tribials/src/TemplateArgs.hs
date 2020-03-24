{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TemplateArgs where

import Language.Haskell.TH

import System.Environment

do	runIO $ getArgs >>= print
	pure []
