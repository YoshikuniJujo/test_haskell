{-# LANGUAGE ScopedTypeVariables #-}

module Game (
	runGame,
	drawBlocks,
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

drawBlocks :: [(Int, Int)] -> GtkWidget -> IO ()
drawBlocks bs w = do
	win <- gtkWidgetGetWindow w
	cr <- gdkCairoCreate (cast win)
	mapM_ (uncurry $ drawBlock cr) bs
	cairoFill cr
	cairoDestroy cr

drawBlock :: CairoT -> Int -> Int -> IO ()
drawBlock cr x y = cairoRectangle cr (30 + x' * 15) (15 + y' * 15) 14 14
	where
	[x', y'] = map fromIntegral [x, y]

cast :: (GObject g1, GObject g2) => g1 -> g2
cast = fromJust . castGObject
