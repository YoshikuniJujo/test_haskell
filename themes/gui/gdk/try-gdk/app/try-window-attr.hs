{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	let	opts = [optTitle, optEvents, optPosition]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	_ <- gdkDisplayOpen ""
	putStrLn `mapM_` es
	w <- gdkWindowNew Nothing $ optsToAttr ss
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkConfigure (gdkEventConfigure -> c) -> True <$ print c
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
				pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data OptSetting
	= OptTitle String
	| OptEvents [GdkEventMaskSingleBit]
	| OptPosition (CInt, CInt)
	deriving Show

optTitle, optEvents, optPosition :: OptDescr OptSetting
optTitle = Option ['t'] ["title"] (ReqArg OptTitle "Title") "Set title"

optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "Event masks")
	"Set event mask"

optPosition = Option ['p'] ["position"] (ReqArg (OptPosition . read) "(X,Y)")
	"Set position"

optsToAttr :: [OptSetting] -> GdkWindowAttr
optsToAttr [] = minimalGdkWindowAttr
	(gdkEventMaskMultiBits [GdkKeyPressMask])
	100 100 GdkInputOutput GdkWindowToplevel
optsToAttr (OptTitle t : ss) = (optsToAttr ss) { gdkWindowAttrTitle = Just t }
optsToAttr (OptEvents es : ss) =
	(optsToAttr ss) { gdkWindowAttrEventMask = gdkEventMaskMultiBits es }
optsToAttr (OptPosition (x, y) : ss) =
	(optsToAttr ss) { gdkWindowAttrX = Just x, gdkWindowAttrY = Just y }
