{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Foldable
import System.Environment
import System.Console.GetOpt
import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkSeat
import Graphics.Gdk.GdkDevice.Internal
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.Windows
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Try.Tools
import Try.Tools.DoWhile

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [
		optDispAndSeat, optList, optIdentity, optAxes, optGeometry ] <$> getArgs
	putStrLn `mapM_` es
	dpy <- gdkDisplayOpen ""
	let	scr = gdkDisplayGetDefaultScreen dpy
	st <- gdkDisplayGetDefaultSeat dpy
	pnt <- gdkSeatGetPointer st
	kbd <- gdkSeatGetKeyboard st

	if OptDisplayAndSeat `notElem` ss then pure () else do
		print dpy
		print scr
		print st
		print pnt
		print kbd

		print =<< gdkDeviceGetDisplay pnt
		print =<< gdkDeviceGetDisplay kbd
		print =<< gdkDeviceGetSeat pnt
		print =<< gdkDeviceGetSeat kbd
		putStrLn ""

	pnts <- gdkDeviceListSlaveDevices pnt
	kbds <- gdkDeviceListSlaveDevices kbd

	if OptList `notElem` ss then pure () else do
		putStrLn =<< gdkDeviceGetName pnt
		for_ pnts \ps -> putStrLn . ('\t' :) =<< gdkDeviceGetName ps
		putStrLn ""
		putStrLn =<< gdkDeviceGetName kbd
		for_ kbds \ks -> putStrLn . ('\t' :) =<< gdkDeviceGetName ks

	sequence_ $ (<$> ss) (flip withOptIdentity \nm -> do
		printDeviceIf nm pnt
		printDevicePhysicalIf nm `mapM_` pnts
		printDeviceIf nm kbd
		printDevicePhysicalIf nm `mapM_` kbds)

	if OptGeometry `notElem` ss then pure () else do

		w <- gdkWindowNew Nothing defaultGdkWindowAttr
		gdkWindowShow w
		gdkDisplayFlush dpy
		gdkDisplaySync dpy

		doWhile_ $ gdkWithEventGet \case
			Nothing -> pure False
			Just (GdkEventGdkAny (gdkEventAny -> e)) -> print e >> pure True

		print =<< gdkDeviceGetPosition pnt
		print =<< gdkDeviceGetPositionDouble pnt
		print =<< gdkDeviceGetWindowAtPosition pnt
		print =<< gdkDeviceGetWindowAtPositionDouble pnt
		gdkDeviceWarp pnt scr 1450 150
		gdkDisplayFlush dpy
		print =<< gdkDeviceGetPosition pnt
		print =<< gdkDeviceGetPositionDouble pnt
		print =<< gdkDeviceGetWindowAtPosition pnt
		print =<< gdkDeviceGetWindowAtPositionDouble pnt

		print =<< gdkDeviceGetLastEventWindow pnt

	if OptAxes `notElem` ss then pure () else do

		print =<< gdkDeviceGetNAxes pnt
		for_ pnts \ps -> print =<< gdkDeviceGetNAxes ps

		print =<< mapM gdkAtomName =<< gdkDeviceListAxes pnt
		for_ pnts \ps ->
			print =<< mapM gdkAtomName =<< gdkDeviceListAxes ps

printDeviceIf :: IsGdkDevice d => String -> d pk -> IO ()
printDeviceIf n0 d = do
	n <- gdkDeviceGetName d
	if n == n0 then printDevice d else pure ()

printDevice :: IsGdkDevice d => d pk -> IO ()
printDevice d = do
	n <- gdkDeviceGetName d
	t <- gdkDeviceGetDeviceType d
	s <- gdkDeviceGetSource d
	putStrLn n
	putStrLn $ '\t' : show t
	putStrLn $ '\t' : show s

printDevicePhysicalIf :: String -> GdkDevicePhysical pk -> IO ()
printDevicePhysicalIf n0 d = do
	n <- gdkDeviceGetName d
	if n == n0 then printDevicePhysical d else pure ()

printDevicePhysical :: GdkDevicePhysical pk -> IO ()
printDevicePhysical d = do
	printDevice d
	let	v = gdkDeviceGetVendorId d
		p = gdkDeviceGetProductId d
	putStrLn $ '\t' : show v
	putStrLn $ '\t' : show p

data OptSetting
	= OptIdentity String
	| OptList
	| OptAxes
	| OptGeometry
	| OptDisplayAndSeat
	deriving (Eq, Show)

withOptIdentity :: OptSetting -> (String -> IO a) -> IO ()
withOptIdentity (OptIdentity n) f = void $ f n
withOptIdentity _ _ = pure ()

optDispAndSeat, optList, optIdentity, optAxes, optGeometry :: OptDescr OptSetting
optDispAndSeat = Option ['d'] ["display-and-seat"] (NoArg OptDisplayAndSeat)
	"Show display and seat"

optList = Option ['l'] ["list"] (NoArg OptList) "Show devices"

optIdentity = Option ['i'] ["identity"] (ReqArg OptIdentity "device name")
	"Show identity"

optAxes = Option ['a'] ["axes"] (NoArg OptAxes) "Show axes"

optGeometry = Option ['g'] ["geometry"] (NoArg OptGeometry) "Show geometry"
