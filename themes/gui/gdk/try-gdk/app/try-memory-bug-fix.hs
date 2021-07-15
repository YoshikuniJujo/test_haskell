{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Foldable
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkModifierType
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Try.Tools

main :: IO ()
main = join (gdkInit <$> getProgName <*> getArgs) >>= \case
	(_, [n]) -> for_ [0 .. read n :: Int] \i -> do
		p <- gdkEventNew'
		putStrLn $ show i ++ ": " ++ show p
		gdkEventFree p
	_ -> error "bad"
