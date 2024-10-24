{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Followbox.RunGtkField (
	runFollowbox, runFollowboxGen, handleFollowbox) where

import Control.Moffy
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Concurrent.STM
import Data.Type.Set ((:-), (:+:))
import System.Random
import Graphics.Gtk
import Graphics.Cairo
import Graphics.CairoType

import Control.Moffy.View.GtkField
import Data.OneOfThem

import Trial.Followbox.Event
import Trial.Followbox.Handle
import Trial.Followbox.ViewType
import Trial.Followbox.TypeSynonym

import Control.Moffy.Run.GtkField (runGtkMain)
import Control.Moffy.Run.GtkField qualified as G

import Control.Moffy.Event.Window
import Control.Moffy.Event.Cursor
import Data.Map

import Foreign.Ptr
import qualified System.Gobject.Hierarchy as New
import qualified System.Gobject.TryDeleteEvent as New

handleFollowbox ::
	(TChan (EvReqs (CursorEv :+: CalcTextExtents :- GuiEv)), TChan (EvOccs (CursorEv :+: CalcTextExtents :- GuiEv))) -> Browser ->
	Maybe GithubNameToken -> HandleF IO (CursorEv :+: CalcTextExtents :- DefaultWindowEv :+: GuiEv :+: FollowboxEv)
handleFollowbox = handleFollowboxWith (uncurry . handle)

runFollowbox :: Browser -> Maybe GithubNameToken -> Sig s (CursorEv :+: StoreDefaultWindow :- FollowboxEv) (Map WindowId View) r -> IO r
runFollowbox brs mgnt s = do
	([], (cr, c, c')) <- runGtkMain drawFollowboxGtk []
	r <- runFollowboxGen cr c brs mgnt c' s
	r <$ gtkMainQuit

runFollowboxGen ::
	TChan (EvReqs G.GuiEv) -> TChan (EvOccs G.GuiEv) -> String ->
	Maybe GithubNameToken -> TChan x ->
	Sig s (CursorEv :+: StoreDefaultWindow :- FollowboxEv) x r ->
	IO r
runFollowboxGen cr c brs mgnt c' s = do
	(r, _) <- interpretSt (handleFollowbox (cr, c) brs mgnt) c' s (initialFollowboxState $ mkStdGen 8)
	pure r

newToOldDrawingArea :: New.GtkDrawingArea -> IO GtkWidget
newToOldDrawingArea da = New.pointer da $ pure . GtkWidget . castPtr

drawFollowboxGtk :: New.GtkDrawingArea -> CairoT -> View -> IO ()
drawFollowboxGtk wdt_ cr (View v) = do
	wdt <- newToOldDrawingArea wdt_
	w <- gtkWidgetGetAllocatedWidth wdt
	h <- gtkWidgetGetAllocatedHeight wdt
	cairoSetSourceRgb cr 0 0 0
	cairoRectangle cr 0 0 (fromIntegral w) (fromIntegral h)
	cairoStrokePreserve cr
	cairoFill cr
	((drawText wdt cr >-- drawLine wdt cr >-- SingletonFun (drawImage wdt cr)) `apply`) `mapM_` v
