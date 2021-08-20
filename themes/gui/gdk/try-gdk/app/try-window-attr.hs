{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

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
	let	opts = [optTitle]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	_ <- gdkDisplayOpen ""
	putStrLn `mapM_` es
	w <- gdkWindowNew Nothing $ optsToAttr ss
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
				pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data OptSetting
	= OptTitle String
	deriving Show

optTitle :: OptDescr OptSetting
optTitle = Option ['t'] ["title"] (ReqArg OptTitle "Title") "Set title"

optsToAttr :: [OptSetting] -> GdkWindowAttr
optsToAttr [] = minimalGdkWindowAttr
	(gdkEventMaskMultiBits [GdkKeyPressMask])
	100 100 GdkInputOutput GdkWindowToplevel
optsToAttr (OptTitle t : ss) = (optsToAttr ss) { gdkWindowAttrTitle = Just t }
