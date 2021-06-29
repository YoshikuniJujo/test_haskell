{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Word
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.Windows
import Graphics.Gdk.Events
import Graphics.Gdk.Cursors
import Graphics.Gdk.Values
import Try.Tools

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	d <- gdkWindowGetDisplay w
	gdkWindowShow w
	gdkWindowSetEventCompression w False
	gdkWindowSetEvents w [gdkPointerMotionMask, gdkButtonPressMask, gdkButtonReleaseMask, gdkKeyPressMask] -- , gdkAllEventsMask]
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		e@(GdkEventGdkMotionNotify _) -> True <$ do
			sd <- gdkEventGetSourceDevice e
			mis <- maybe (pure Nothing) ((Just <$>) . gdkDeviceGetSource)  sd
			case mis of
				Nothing -> pure ()
				Just is	| is == GdkSourceMouse -> gdkWindowSetCursor w =<< gdkCursorNewFromName d "wait"
					| is == GdkSourcePen -> gdkWindowSetCursor w =<< gdkCursorNewFromName d "text"
					| is == GdkSourceTouchpad -> gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
					| otherwise -> pure ()
		GdkEventGdkKeyPress k -> do
			kv <- gdkEventKeyKeyval k
			gdkWindowSetCursor w =<< gdkCursorNewForDisplay d (cursorType kv)
			pure $ kv /= fromIntegral (ord 'q')
		_ -> pure True

cursorType :: Word32 -> GdkCursorType
cursorType kv
	| kv == toKeyval 'a' = GdkXCursor
	| kv == toKeyval 'b' = GdkArrow
	| kv == toKeyval 'c' = GdkBasedArrowDown
	| kv == toKeyval 'd' = GdkBasedArrowUp
	| kv == toKeyval 'e' = GdkBoat
	| kv == toKeyval 'f' = GdkBogosity
	| kv == toKeyval 'g' = GdkBottomLeftCorner
	| kv == toKeyval 'h' = GdkBottomRightCorner
	| otherwise = GdkXCursor

toKeyval :: Char -> Word32
toKeyval = fromIntegral . ord
