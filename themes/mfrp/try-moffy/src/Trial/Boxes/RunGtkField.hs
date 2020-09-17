{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.RunGtkField (runBoxes) where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Handle.TChan
import Control.Moffy.Handle.Time
import Control.Moffy.Viewable.Shape
import Control.Moffy.View.GtkField
import Control.Moffy.Run
import Control.Concurrent.STM
import Data.Time.Clock.System
import Graphics.Gtk

import Control.Moffy.Event.Time
import Data.Time
import Data.Time.Clock.TAI
import Data.Type.Set

import Trial.Boxes.BoxEv

import Control.Moffy.Handle
import Control.Moffy.Run.GtkField

runBoxes :: SigB s [Box] r -> IO r
runBoxes s = do
	([], (cr, c, c')) <- runGtkMain (\w cr -> (drawBox w cr `mapM_`)) []
	(r, _) <- interpretSt (handleBoxesFoo 0.1 cr c) (atomically . writeTChan c') s . (InitialMode ,) . systemToTAITime =<< getSystemTime
	r <$ gtkMainQuit

handleBoxesFoo :: DiffTime -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	HandleSt (Mode, AbsoluteTime) IO (TimeEv :+: GuiEv)
handleBoxesFoo dt cr co = retrySt
	$ ((\f x y z -> f (x, (y, z))) . popInput . handleTimeEvPlus . pushInput) (\(x, (y, z)) -> (((liftHandle' .) .) . handle . Just) x y z) dt cr co
