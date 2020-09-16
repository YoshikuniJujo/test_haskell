{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Handle.GtkField
import Control.Moffy.Handle.Time
import Control.Moffy.Run
import Control.Concurrent.STM
import Data.Time.Clock.System
import Graphics.Gtk

import Trial.Boxes.BoxEv
import Trial.Boxes.GtkField

import Trial.Boxes

runBoxes :: SigB s [Box] r -> IO r
runBoxes s = do
	(cr, c, c') <- tryUseTChanGen \w cr -> (drawBox w cr `mapM_`)
	(r, _) <- interpretSt (handleBoxesFoo 0.1 cr c) (atomically . writeTChan c') s . (InitialMode ,) . systemToTAITime =<< getSystemTime
	r <$ gtkMainQuit

main :: IO ()
main = () <$ runBoxes (boxes `break` deleteEvent)
