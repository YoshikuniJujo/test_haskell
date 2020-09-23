{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.Draw
import Trial.Draw.Viewable

main :: IO ()
main = print . snd =<< runDraw
	(\wdt cr x -> ((drawBox wdt cr >-- drawLine wdt cr >-- fillPolygon wdt cr >-- SingletonFun putMessage) `apply`) `mapM_` x)
	(rectangleAndLines `break` deleteEvent)
