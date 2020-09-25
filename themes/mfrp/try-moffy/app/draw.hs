{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.View.GtkField
import Data.OneOfThem
import Data.Maybe

import Trial.Draw
import Trial.Draw.Viewable

import qualified Trial.Draw.Marshal as M

import System.Environment

main :: IO ()
main = do
	r : w : _ <- getArgs
	Right s <- case r of
		"-" -> pure $ Right []
		_ -> M.readFile r
	s' <- either (error "bad") (maybe (error "bad") (mapMaybe viewableToShape) . fst) . fst <$> runDraw
		(\wdt cr x -> ((drawBox wdt cr >-- drawLine wdt cr >-- fillPolygon wdt cr >-- SingletonFun putMessage) `apply`) `mapM_` x) do
			i <- waitFor $ adjust windowNew
			rectangleAndLines s `break` deleteEvent i
	M.writeFile w s'
