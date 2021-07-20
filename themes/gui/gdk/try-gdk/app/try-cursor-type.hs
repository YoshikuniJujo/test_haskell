{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Char
import Data.IORef
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.Cursors
import Try.Tools

main :: IO ()
main = do
	cnt <- newIORef 0
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	w <- gdkWindowNew Nothing defaultGdkWindowAttr
	d <- gdkWindowGetDisplay w
	gdkWindowShow w
	gdkWindowSetEventCompression w False
	gdkWindowSetEvents w $ gdkEventMaskMultiBits [
		GdkPointerMotionMask, GdkButtonPressMask, GdkButtonReleaseMask,
		GdkKeyPressMask ] -- , gdkAllEventsMask]
	gdkWindowSetCursor w =<< gdkCursorNewFromName d "crosshair"
	mainLoopNew \case
		GdkEventGdkDelete _d -> pure False
		GdkEventSealedGdkMotionNotify _ -> pure True
		GdkEventGdkKeyPress k -> do
			let	kv = gdkEventKeyKeyval $ gdkEventKey k
			print kv
			when (kv == GdkKeySym 65505 || kv == GdkKeySym 65506) $ modifyIORef cnt (+ 1)
			n <- readIORef cnt
			c <- gdkCursorNewForDisplay d $ cursorType n kv
			print =<< gdkCursorGetCursorType c
			gdkWindowSetCursor w c
			pure $ kv == GdkKey_q
		_ -> pure True

cursorType :: Int -> GdkKeySym -> GdkCursorType
cursorType n = case n `mod` 4 of
	0 -> cursorType0; 1 -> cursorType1; 2 -> cursorType2; 3 -> cursorType3
	_ -> error "never occur"

cursorType0 :: GdkKeySym -> GdkCursorType
cursorType0 kv
	| checkKeyVal kv 'a' = GdkXCursor
	| checkKeyVal kv 'b' = GdkArrow
	| checkKeyVal kv 'c' = GdkBasedArrowDown
	| checkKeyVal kv 'd' = GdkBasedArrowUp
	| checkKeyVal kv 'e' = GdkBoat
	| checkKeyVal kv 'f' = GdkBogosity
	| checkKeyVal kv 'g' = GdkBottomLeftCorner
	| checkKeyVal kv 'h' = GdkBottomRightCorner
	| checkKeyVal kv 'i' = GdkBottomSide
	| checkKeyVal kv 'j' = GdkBottomTee
	| checkKeyVal kv 'k' = GdkBoxSpiral
	| checkKeyVal kv 'l' = GdkCenterPtr
	| checkKeyVal kv 'm' = GdkCircle
	| checkKeyVal kv 'n' = GdkClock
	| checkKeyVal kv 'o' = GdkCoffeeMug
	| checkKeyVal kv 'p' = GdkCross
	| checkKeyVal kv 'r' = GdkCrossReverse
	| checkKeyVal kv 's' = GdkDiamondCross
	| checkKeyVal kv 't' = GdkDot
	| checkKeyVal kv 'u' = GdkDotbox
	| checkKeyVal kv 'v' = GdkDoubleArrow
	| checkKeyVal kv 'w' = GdkDraftLarge
	| checkKeyVal kv 'x' = GdkDraftSmall
	| checkKeyVal kv 'y' = GdkDrapedBox
	| checkKeyVal kv 'z' = GdkExchange
	| otherwise = GdkXCursor

cursorType1 :: GdkKeySym -> GdkCursorType
cursorType1 kv
	| checkKeyVal kv 'a' = GdkFleur
	| checkKeyVal kv 'b' = GdkGobbler
	| checkKeyVal kv 'c' = GdkGumby
	| checkKeyVal kv 'd' = GdkHand1
	| checkKeyVal kv 'e' = GdkHand2
	| checkKeyVal kv 'f' = GdkHeart
	| checkKeyVal kv 'g' = GdkIcon
	| checkKeyVal kv 'h' = GdkIronCross
	| checkKeyVal kv 'i' = GdkLeftPtr
	| checkKeyVal kv 'j' = GdkLeftSide
	| checkKeyVal kv 'k' = GdkLeftTee
	| checkKeyVal kv 'l' = GdkLeftbutton
	| checkKeyVal kv 'm' = GdkLlAngle
	| checkKeyVal kv 'n' = GdkLrAngle
	| checkKeyVal kv 'o' = GdkMan
	| checkKeyVal kv 'p' = GdkMiddlebutton
	| checkKeyVal kv 'r' = GdkMouse
	| checkKeyVal kv 's' = GdkPencil
	| checkKeyVal kv 't' = GdkPirate
	| checkKeyVal kv 'u' = GdkPlus
	| checkKeyVal kv 'v' = GdkQuestionArrow
	| checkKeyVal kv 'w' = GdkRightPtr
	| checkKeyVal kv 'x' = GdkRightSide
	| checkKeyVal kv 'y' = GdkRightTee
	| checkKeyVal kv 'z' = GdkRightbutton
	| otherwise = GdkFleur

cursorType2 :: GdkKeySym -> GdkCursorType
cursorType2 kv
	| checkKeyVal kv 'a' = GdkRtlLogo
	| checkKeyVal kv 'b' = GdkSailboat
	| checkKeyVal kv 'c' = GdkSbDownArrow
	| checkKeyVal kv 'd' = GdkSbHDoubleArrow
	| checkKeyVal kv 'e' = GdkSbLeftArrow
	| checkKeyVal kv 'f' = GdkSbRightArrow
	| checkKeyVal kv 'g' = GdkSbUpArrow
	| checkKeyVal kv 'h' = GdkSbVDoubleArrow
	| checkKeyVal kv 'i' = GdkShuttle
	| checkKeyVal kv 'j' = GdkSizing
	| checkKeyVal kv 'k' = GdkSpider
	| checkKeyVal kv 'l' = GdkSpraycan
	| checkKeyVal kv 'm' = GdkStar
	| checkKeyVal kv 'n' = GdkTarget
	| checkKeyVal kv 'o' = GdkTcross
	| checkKeyVal kv 'p' = GdkTopLeftArrow
	| checkKeyVal kv 'r' = GdkTopLeftCorner
	| checkKeyVal kv 's' = GdkTopRightCorner
	| checkKeyVal kv 't' = GdkTopSide
	| checkKeyVal kv 'u' = GdkTopTee
	| checkKeyVal kv 'v' = GdkTrek
	| checkKeyVal kv 'w' = GdkUlAngle
	| checkKeyVal kv 'x' = GdkUmbrella
	| checkKeyVal kv 'y' = GdkUrAngle
	| checkKeyVal kv 'z' = GdkWatch
	| otherwise = GdkRtlLogo

cursorType3 :: GdkKeySym -> GdkCursorType
cursorType3 kv
	| checkKeyVal kv 'a' = GdkXterm
--	| checkKeyVal kv 'b' = GdkLastCursor
	| checkKeyVal kv 'c' = GdkBlankCursor
--	| checkKeyVal kv 'd' = GdkCursorIsPixmap
	| otherwise = GdkXterm

toKeyval :: Char -> GdkKeySym
toKeyval = GdkKeySym . fromIntegral . ord

checkKeyVal :: GdkKeySym -> Char -> Bool
checkKeyVal kv c = kv == toKeyval c
