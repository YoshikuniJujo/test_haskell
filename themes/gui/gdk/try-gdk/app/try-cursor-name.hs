{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import Data.IORef
import Data.KeySym
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.Cursors
import Try.Tools

main :: IO ()
main = do
	cnt <- newIORef 0
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkToplevelNew Nothing defaultGdkWindowAttr
	let	d = gdkWindowGetDisplay w
	gdkWindowShow w
	gdkWindowSetEventCompression w False
	gdkWindowSetEvents w $ gdkEventMaskMultiBits [
		GdkPointerMotionMask, GdkButtonPressMask, GdkButtonReleaseMask,
		GdkKeyPressMask ] -- , gdkAllEventsMask]
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval <$> gdkEventKey k
			print kv
			when (kv == KeySym 65505 || kv == KeySym 65506) $ modifyIORef cnt (+ 1)
			n <- readIORef cnt
			gdkWindowSetCursor w =<< gdkCursorNewFromName d (cursorName n kv)
			pure $ kv /= KeySym (fromIntegral $ ord 'q')
		_ -> pure True

cursorName :: Int -> KeySym -> String
cursorName n = case n `mod` 2 of
	0 -> cursorName0; 1 -> cursorName1; _ -> error "never occur"

cursorName0 :: KeySym -> String
cursorName0 kv
	| kv == toKeyval 'a' = "none"
	| kv == toKeyval 'b' = "default"
	| kv == toKeyval 'c' = "help"
	| kv == toKeyval 'd' = "pointer"
	| kv == toKeyval 'e' = "context-menu"
	| kv == toKeyval 'f' = "progress"
	| kv == toKeyval 'g' = "wait"
	| kv == toKeyval 'h' = "cell"
	| kv == toKeyval 'i' = "crosshair"
	| kv == toKeyval 'j' = "text"
	| kv == toKeyval 'k' = "vertical-text"
	| kv == toKeyval 'l' = "alias"
	| kv == toKeyval 'm' = "copy"
	| kv == toKeyval 'n' = "no-drop"
	| kv == toKeyval 'o' = "move"
	| kv == toKeyval 'p' = "not-allowed"
	| kv == toKeyval 'r' = "grab"
	| kv == toKeyval 's' = "grabbing"
	| kv == toKeyval 't' = "all-scroll"
	| kv == toKeyval 'u' = "col-resize"
	| kv == toKeyval 'v' = "row-resize"
	| kv == toKeyval 'w' = "n-resize"
	| kv == toKeyval 'x' = "e-resize"
	| kv == toKeyval 'y' = "s-resize"
	| kv == toKeyval 'z' = "w-resize"
	| otherwise = "default"

cursorName1 :: KeySym -> String
cursorName1 kv
	| kv == toKeyval 'a' = "ne-resize"
	| kv == toKeyval 'b' = "nw-resize"
	| kv == toKeyval 'c' = "sw-resize"
	| kv == toKeyval 'd' = "se-resize"
	| kv == toKeyval 'e' = "ew-resize"
	| kv == toKeyval 'f' = "ns-resize"
	| kv == toKeyval 'g' = "nesw-resize"
	| kv == toKeyval 'h' = "nwse-resize"
	| kv == toKeyval 'i' = "zoom-in"
	| kv == toKeyval 'j' = "zoom-out"
	| otherwise = "ne-resize"

toKeyval :: Char -> KeySym
toKeyval = KeySym . fromIntegral . ord
