{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Word
import Data.Char
import Data.IORef
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
	cnt <- newIORef 0
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
			print kv
			when (kv == 65505 || kv == 65506) $ modifyIORef cnt (+ 1)
			n <- readIORef cnt
			gdkWindowSetCursor w =<< gdkCursorNewForDisplay d (cursorType n kv)
			pure $ kv /= fromIntegral (ord 'q')
		_ -> pure True

cursorType :: Int -> Word32 -> GdkCursorType
cursorType n = case n `mod` 4 of
	0 -> cursorType0; 1 -> cursorType1; 2 -> cursorType2; 3 -> cursorType3
	_ -> error "never occur"

cursorType0 :: Word32 -> GdkCursorType
cursorType0 kv
	| kv == toKeyval 'a' = GdkXCursor
	| kv == toKeyval 'b' = GdkArrow
	| kv == toKeyval 'c' = GdkBasedArrowDown
	| kv == toKeyval 'd' = GdkBasedArrowUp
	| kv == toKeyval 'e' = GdkBoat
	| kv == toKeyval 'f' = GdkBogosity
	| kv == toKeyval 'g' = GdkBottomLeftCorner
	| kv == toKeyval 'h' = GdkBottomRightCorner
	| kv == toKeyval 'i' = GdkBottomSide
	| kv == toKeyval 'j' = GdkBottomTee
	| kv == toKeyval 'k' = GdkBoxSpiral
	| kv == toKeyval 'l' = GdkCenterPtr
	| kv == toKeyval 'm' = GdkCircle
	| kv == toKeyval 'n' = GdkClock
	| kv == toKeyval 'o' = GdkCoffeeMug
	| kv == toKeyval 'p' = GdkCross
	| kv == toKeyval 'r' = GdkCrossReverse
	| kv == toKeyval 's' = GdkDiamondCross
	| kv == toKeyval 't' = GdkDot
	| kv == toKeyval 'u' = GdkDotbox
	| kv == toKeyval 'v' = GdkDoubleArrow
	| kv == toKeyval 'w' = GdkDraftLarge
	| kv == toKeyval 'x' = GdkDraftSmall
	| kv == toKeyval 'y' = GdkDrapedBox
	| kv == toKeyval 'z' = GdkExchange
	| otherwise = GdkXCursor

cursorType1 :: Word32 -> GdkCursorType
cursorType1 kv
	| kv == toKeyval 'a' = GdkFleur
	| kv == toKeyval 'b' = GdkGobbler
	| kv == toKeyval 'c' = GdkGumby
	| kv == toKeyval 'd' = GdkHand1
	| kv == toKeyval 'e' = GdkHand2
	| kv == toKeyval 'f' = GdkHeart
	| kv == toKeyval 'g' = GdkIcon
	| kv == toKeyval 'h' = GdkIronCross
	| kv == toKeyval 'i' = GdkLeftPtr
	| kv == toKeyval 'j' = GdkLeftSide
	| kv == toKeyval 'k' = GdkLeftTee
	| kv == toKeyval 'l' = GdkLeftbutton
	| kv == toKeyval 'm' = GdkLlAngle
	| kv == toKeyval 'n' = GdkLrAngle
	| kv == toKeyval 'o' = GdkMan
	| kv == toKeyval 'p' = GdkMiddlebutton
	| kv == toKeyval 'r' = GdkMouse
	| kv == toKeyval 's' = GdkPencil
	| kv == toKeyval 't' = GdkPirate
	| kv == toKeyval 'u' = GdkPlus
	| kv == toKeyval 'v' = GdkQuestionArrow
	| kv == toKeyval 'w' = GdkRightPtr
	| kv == toKeyval 'x' = GdkRightSide
	| kv == toKeyval 'y' = GdkRightTee
	| kv == toKeyval 'z' = GdkRightbutton
	| otherwise = GdkFleur

cursorType2 :: Word32 -> GdkCursorType
cursorType2 kv
	| kv == toKeyval 'a' = GdkRtlLogo
	| kv == toKeyval 'b' = GdkSailboat
	| kv == toKeyval 'c' = GdkSbDownArrow
	| kv == toKeyval 'd' = GdkSbHDoubleArrow
	| kv == toKeyval 'e' = GdkSbLeftArrow
	| kv == toKeyval 'f' = GdkSbRightArrow
	| kv == toKeyval 'g' = GdkSbUpArrow
	| kv == toKeyval 'h' = GdkSbVDoubleArrow
	| kv == toKeyval 'i' = GdkShuttle
	| kv == toKeyval 'j' = GdkSizing
	| kv == toKeyval 'k' = GdkSpider
	| kv == toKeyval 'l' = GdkSpraycan
	| kv == toKeyval 'm' = GdkStar
	| kv == toKeyval 'n' = GdkTarget
	| kv == toKeyval 'o' = GdkTcross
	| kv == toKeyval 'p' = GdkTopLeftArrow
	| kv == toKeyval 'r' = GdkTopLeftCorner
	| kv == toKeyval 's' = GdkTopRightCorner
	| kv == toKeyval 't' = GdkTopSide
	| kv == toKeyval 'u' = GdkTopTee
	| kv == toKeyval 'v' = GdkTrek
	| kv == toKeyval 'w' = GdkUlAngle
	| kv == toKeyval 'x' = GdkUmbrella
	| kv == toKeyval 'y' = GdkUrAngle
	| kv == toKeyval 'z' = GdkWatch
	| otherwise = GdkRtlLogo

cursorType3 :: Word32 -> GdkCursorType
cursorType3 kv
	| kv == toKeyval 'a' = GdkXterm
--	| kv == toKeyval 'b' = GdkLastCursor
	| kv == toKeyval 'c' = GdkBlankCursor
--	| kv == toKeyval 'd' = GdkCursorIsPixmap
	| otherwise = GdkXterm

toKeyval :: Char -> Word32
toKeyval = fromIntegral . ord
