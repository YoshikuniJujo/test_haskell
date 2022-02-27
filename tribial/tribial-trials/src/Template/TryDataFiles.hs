{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Template.TryDataFiles where

import Language.Haskell.TH

import Paths_tribial_trials

runIO do
	putStrLn "\n*** TEMPLATE TRY DATA FILES ***"
	print =<< readFile =<< getDataFileName "th/some.txt"
	putStrLn ""
	pure []
