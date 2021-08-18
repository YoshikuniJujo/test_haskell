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
	let	opts = [optDelta]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	let	delta = getDelta ss
	_dpy <- gdkDisplayOpen ""
	w <- gdkWindowNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [
			GdkKeyPressMask
			])
		900 700 GdkInputOutput GdkWindowToplevel
	gdkWindowShow w
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> c) -> True <$ do
				let	(dx, dy) = case c of
						GdkKey_h -> (- delta, 0)
						GdkKey_j -> (0, delta)
						GdkKey_k -> (0, - delta)
						GdkKey_l -> (delta, 0)
						_ -> (0, 0)
				(x, y) <- gdkWindowGetRootOrigin w
				gdkWindowMove w (x + dx) (y + dy)
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data Setting = Delta CInt deriving Show

optDelta :: OptDescr Setting
optDelta = Option ['d'] ["delta"] (ReqArg (Delta . read) "Delta") "Set delta"

getDelta :: [Setting] -> CInt
getDelta [] = 1
getDelta (Delta d : _) = d
getDelta (_ : ss) = getDelta ss
