{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.TryFillPolygon

main :: IO ()
main = void $ runFillPolygon (\wdt cr x -> ((drawBox wdt cr >-- SingletonFun (fillPolygon wdt cr)) `apply`) `mapM_` x) do
	i <- waitFor $ adjust windowNew
	tryFillPolygon `break` deleteEvent i
