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
	let	opts = [optEvents]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	print ss
	_ <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits $ optsToEventMaskList ss) 400 300
	gdkWindowShow win
	mainLoop \case
		GdkEventGdkNothing (gdkEventAny -> e) -> True <$ print e
		GdkEventGdkDelete (gdkEventAny -> e) -> True <$ (print e >> gdkWindowDestroy win)
		GdkEventGdkDestroy (gdkEventAny -> e) -> False <$ print e
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
				True <$ gdkWindowDestroy win
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data OptSetting
	= OptEvents [GdkEventMaskSingleBit]
	deriving Show

optEvents :: OptDescr OptSetting
optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "EventMask")
	"Set event mask"

optsToEventMaskList :: [OptSetting] -> [GdkEventMaskSingleBit]
optsToEventMaskList = \case
	[] -> [GdkKeyPressMask]
	OptEvents ems : _ -> ems
