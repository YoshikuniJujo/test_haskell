{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Moffy
import Control.Moffy.Event.Window

import Trial.Followbox
import Trial.Followbox.RunGtkField

import qualified Data.Map as Map
import Data.Type.Flip

main :: IO ()
main = runFollowbox "firefox" Nothing do
	i <- waitFor $ adjust windowNew
	Map.singleton i <$%> followbox i
