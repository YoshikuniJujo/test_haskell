{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryMultiWindow where

import Prelude hiding (break)

import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key
import Control.Moffy.Handle
import Control.Moffy.Handle.TChan
import Control.Moffy.Viewable.Shape
import Control.Moffy.View.GtkField
import Control.Moffy.Run.GtkField
import Control.Moffy.Run.TChan
import Data.Type.Set
import Data.Type.Flip
import Data.Bool

import Data.OneOfThem as Oot
import qualified Data.Map as Map

threeWindows :: Sig s GuiEv (Map.Map WindowId [OneOfThem (Singleton Box)]) (WindowId, WindowId, WindowId)
threeWindows = do
	emit $ Map.fromList [
		(WindowId 0, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Blue),
		(WindowId 1, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Red),
		(WindowId 2, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Cyan) ]
	w0 <- waitFor $ adjust windowNew
	emit $ Map.fromList [
		(w0, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Green),
		(WindowId 1, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Yellow),
		(WindowId 2, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Magenta) ]
	w1 <- waitFor $ adjust windowNew
	emit $ Map.fromList [
		(w0, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Green),
		(w1, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Yellow) ]
	w2 <- waitFor $ adjust windowNew
--	emit . Map.singleton w2 . (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (100, 100)) Cyan
	emit $ Map.fromList [
		(w0, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (200, 200)) Green),
		(w1, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (200, 200)) Yellow),
		(w2, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (200, 200)) Magenta) ]
	(waitFor . find (\(x, y, z) -> not $ x || y || z) $ (,,) <$%> ddw w0 <*%> ddw w1 <*%> ddw w2) `break` pressQ
	pure (w0, w1, w2)

pressQ :: React s (Singleton KeyDown) ()
pressQ = bool pressQ (pure ()) . isQ =<< keyDown

isQ :: Key -> Bool
isQ (AsciiKey 'q') = True
isQ _ = False

ddw :: WindowId -> Sig s GuiEv Bool ()
ddw i = do
	emit True
	waitFor do
		adjust $ deleteEvent i
		adjust $ windowDestroy i
	emit False
	waitFor never

runTryMultiWindow :: Adjustable es GuiEv => Sig s es (Map.Map WindowId [OneOfThem (Singleton Box)]) r -> IO r
runTryMultiWindow s = do
	([], (cr, c, c')) <- runGtkMain (\wdt cr -> mapM_ (SingletonFun (drawBox wdt cr) `apply`)) []
	interpret (retry $ handle Nothing cr c) c' s
