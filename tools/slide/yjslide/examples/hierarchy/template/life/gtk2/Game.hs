{-# LANGUAGE ScopedTypeVariables #-}

module Game (
	GtkWidget,
	runGame,
	drawBlocks,
	showPoint,
) where

import Control.Monad

import Data.Maybe
import Data.IORef

import Gtk

runGame :: Int -> i -> (Keyval -> Maybe i) -> IO s -> (i -> s -> s) ->
	(s -> GtkWidget -> IO ()) -> IO ()
runGame int tick k2i s0 next draw = do
	sr <- newIORef =<< s0
	gtkInit
	w <- gtkWindowNew
	a <- gtkDrawingAreaNew
	gSignalConnect (cast w) "destroy" gtkMainQuit
	gSignalConnect (cast w) "key-press-event" $ \(_w :: GtkWidget) e -> do
		kv <- gdkEventKeyGetKeyval e
		maybe (return ()) (modifyIORef sr . next) $ k2i kv
		when (kv == char2keyval 'q') gtkMainQuit
		gtkWidgetQueueDraw (cast w)
	gSignalConnect (cast a) "draw" (\w -> readIORef sr >>= flip draw w)
	gTimeoutAddSimple int $ do
		modifyIORef sr (next tick)
		gtkWidgetQueueDraw (cast w)
		return True
	gtkContainerAdd (cast w) (cast a)
	gtkWidgetShowAll (cast w)
	gtkMain

drawBlocks :: [((Int, Int), (Double, Double, Double))] -> GtkWidget -> IO ()
drawBlocks bs w = do
	win <- gtkWidgetGetWindow w
	cr <- gdkCairoCreate (cast win)
	mapM_ (drawBlock cr) bs
--	cairoFill cr
	cairoDestroy cr

drawBlock :: CairoT -> ((Int, Int), (Double, Double, Double)) -> IO ()
drawBlock cr ((x, y), (r, g, b)) = do
	cairoSetSourceRGB cr r g b
	cairoRectangle cr (120 + x' * 15) (15 + y' * 15) 14 14
	cairoFill cr
	where
	[x', y'] = map fromIntegral [x, y]

showPoint :: GtkWidget -> Int -> IO ()
showPoint w p = do
	win <- gtkWidgetGetWindow w
	cr <- gdkCairoCreate (cast win)
	cairoSetFontSize cr 20
	cairoMoveTo cr 300 50
	cairoShowText cr $ show p
	cairoDestroy cr

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject
