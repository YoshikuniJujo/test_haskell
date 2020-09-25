{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Trial.Boxes
import Trial.Boxes.RunGtkField

import qualified Data.Map as Map
import Data.Type.Flip

main :: IO ()
main = () <$ runBoxes do
	i <- waitFor $ adjust windowNew
	Map.singleton i <$%> (boxes `break` deleteEvent i)
