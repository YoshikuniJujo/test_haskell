{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.RunGtkField where

import Control.Moffy
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Concurrent.STM
import Data.Type.Set ((:-), (:+:))
import System.Random
import Graphics.Gtk
import Graphics.Gtk.Cairo

import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.Followbox.Event
import Trial.Followbox.Handle
import Trial.Followbox.ViewType
import Trial.Followbox.TypeSynonym

import Control.Moffy.Run.GtkField (runGtkMain)

import Control.Moffy.Event.Window
import Control.Moffy.Event.Cursor
import Data.Map

handleFollowbox ::
	(TChan (EvReqs (CursorEv :+: CalcTextExtents :- GuiEv)), TChan (EvOccs (CursorEv :+: CalcTextExtents :- GuiEv))) -> Browser ->
	Maybe GithubNameToken -> HandleF IO (CursorEv :+: CalcTextExtents :- DefaultWindowEv :+: GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith (uncurry . handle)

runFollowbox :: Browser -> Maybe GithubNameToken -> Sig s (CursorEv :+: StoreDefaultWindow :- FollowboxEv) (Map WindowId View) r -> IO r
runFollowbox brs mgnt s = do
	([], (cr, c, c')) <- runGtkMain drawFollowboxGtk []
	(r, _) <- interpretSt (handleFollowbox (cr, c) brs mgnt) c' s (initialFollowboxState $ mkStdGen 8)
	r <$ gtkMainQuit

drawFollowboxGtk :: GtkWidget -> CairoT -> View -> IO ()
drawFollowboxGtk wdt cr (View v) = do
		w <- gtkWidgetGetAllocatedWidth wdt
		h <- gtkWidgetGetAllocatedHeight wdt
		cairoSetSourceRgb cr 0 0 0
		cairoRectangle cr 0 0 (fromIntegral w) (fromIntegral h)
		cairoStrokePreserve cr
		cairoFill cr
		((drawText wdt cr >-- drawLine wdt cr >-- SingletonFun (drawImage wdt cr)) `apply`) `mapM_` v
