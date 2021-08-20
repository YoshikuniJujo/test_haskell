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
	let	opts = [optDelta, optMoveResize]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	let	delta = getDelta ss
	_dpy <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [
			GdkKeyPressMask
			]) 900 700
	gdkWindowShow win
	mainLoop \case
		GdkEventGdkDelete _d -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) -> pure False
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> c) -> True <$ do
				let	(dx, dy, dw, dh) = case c of
						GdkKey_h -> (- delta, 0, 0, 0)
						GdkKey_j -> (0, delta, 0, 0)
						GdkKey_k -> (0, - delta, 0, 0)
						GdkKey_l -> (delta, 0, 0, 0)
						GdkKey_s -> (0, 0, - delta, 0)
						GdkKey_d -> (0, 0, 0, - delta)
						GdkKey_f -> (0, 0, 0, delta)
						GdkKey_g -> (0, 0, delta, 0)
						_ -> (0, 0, 0, 0)
				(x, y) <- gdkWindowGetRootOrigin win
				w <- gdkWindowGetWidth win
				h <- gdkWindowGetHeight win
				if (OptMoveResize `elem` ss)
				then gdkWindowMoveResize win
					(x + dx) (y + dy) (w + dw) (h + dh)
				else do	gdkWindowMove win (x + dx) (y + dy)
					gdkWindowResize win (w + dw) (h + dh)
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data Setting = OptDelta CInt | OptMoveResize deriving (Show, Eq)

optDelta, optMoveResize :: OptDescr Setting
optDelta = Option ['d'] ["delta"] (ReqArg (OptDelta . read) "Delta") "Set delta"

optMoveResize = Option ['m'] ["move-resize"] (NoArg OptMoveResize)
	"use gdkWindowMoveResize"

getDelta :: [Setting] -> CInt
getDelta [] = 1
getDelta (OptDelta d : _) = d
getDelta (_ : ss) = getDelta ss
