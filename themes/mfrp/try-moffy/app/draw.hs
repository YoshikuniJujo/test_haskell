{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.Draw

main :: IO ()
main = void $ runDraw (\wdt cr x -> ((drawBox wdt cr >-- SingletonFun (drawLine wdt cr)) `apply`) `mapM_` x)
	(rectangleAndLines `break` deleteEvent)
