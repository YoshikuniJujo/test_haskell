{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.TryFillPolygon

main :: IO ()
main = void $ runFillPolygon (\wdt cr x -> (SingletonFun (drawBox wdt cr) `apply`) `mapM_` x)
	(tryFillPolygon `break` deleteEvent)
