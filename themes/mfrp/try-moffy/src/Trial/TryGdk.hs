{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryGdk where

import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMore
import qualified Data.OneOrMoreApp as App
import Data.Map
import Control.Moffy.Event.Window
import Control.Moffy.Handle

import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask

handleWindowNew :: TVar WindowId -> TVar (Map WindowId GdkWindow) -> Handle' IO (Singleton WindowNew)
handleWindowNew nid ws (unSingleton -> WindowNewReq) = do
	w <- gdkToplevelNew Nothing $ minimalGdkWindowAttr (gdkEventMaskMultiBits []) 700 500
	atomically do
		i@(WindowId i') <- readTVar nid
		writeTVar nid $ WindowId (i' + 1)
		modifyTVar ws $ insert i w
		pure . Just . App.Singleton $ OccWindowNew i
--		pure . Just . App.Singleton . OccWindowNew $ WindowId 0
