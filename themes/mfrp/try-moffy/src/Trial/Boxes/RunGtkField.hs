{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.RunGtkField (runBoxes) where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Handle.TChan
import Control.Moffy.Handle.Time
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Viewable.Shape
import Control.Moffy.View.GtkField
import Control.Moffy.Run.TChan
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

import Data.Map

runBoxes :: SigB s (Map WindowId [Box]) r -> IO r
runBoxes s = do
	([], (cr, c, c')) <- runGtkMain (\w cr -> (drawBox w cr `mapM_`)) []
	(r, _) <- interpretSt (retrySt $ handleBoxesFoo 0.1 cr c) c' s . initialBoxesState . systemToTAITime =<< getSystemTime
	r <$ gtkMainQuit

handleBoxes :: DiffTime -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	HandleSt' BoxesState IO (TimeEv :+: DefaultWindowEv :+: GuiEv)
handleBoxes = handleBoxesFoo

initialBoxesState :: AbsoluteTime -> BoxesState
initialBoxesState t = BoxesState {
	bsMode = InitialMode,
	bsLatestTime = t,
	bsDefaultWindow = Nothing
	}

data BoxesState = BoxesState {
	bsMode :: Mode,
	bsLatestTime :: AbsoluteTime,
	bsDefaultWindow :: Maybe WindowId }
	deriving Show

instance TimeState BoxesState where
	getMode = bsMode; putMode s m = s { bsMode = m }
	getLatestTime = bsLatestTime; putLatestTime s lt = s { bsLatestTime = lt }

instance DefaultWindowState BoxesState where
	getDefaultWindow = bsDefaultWindow
	putDefaultWindow s dw = s { bsDefaultWindow = Just dw }

handleBoxesFoo :: DiffTime -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	HandleSt' BoxesState IO (TimeEv :+: DefaultWindowEv :+: GuiEv)
handleBoxesFoo dt cr co = handleDefaultWindow `beforeSt` handleBoxesFooGen  dt cr co

handleBoxesFooGen :: DiffTime -> TChan (EvReqs GuiEv) -> TChan (EvOccs GuiEv) ->
	HandleSt' BoxesState IO (TimeEv :+: GuiEv)
handleBoxesFooGen dt cr co =
	((\f x y z -> f (x, (y, z))) . popInput . handleTimeEvPlus . pushInput) (\(x, (y, z)) -> (((liftHandle' .) .) . handle . Just) x y z) dt cr co
