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
import Data.KeySym

import Data.OneOfThem as Oot
import qualified Data.Map as Map

threeWindows :: Sig s GuiEv (Map.Map WindowId [OneOfThem (Singleton Box)]) (WindowId, WindowId, WindowId)
threeWindows = do
	w0 <- waitFor $ adjust windowNew
	w1 <- waitFor $ adjust windowNew
	w2 <- waitFor $ adjust windowNew
	emit $ Map.fromList [
		(w0, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (200, 200)) Green),
		(w1, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (200, 200)) Yellow),
		(w2, (: []) . Oot.expand . Singleton $ Box (Rect (50, 50) (200, 200)) Magenta) ]
	_ <- (waitFor . find (\(x, y, z) -> not $ x || y || z) $ (,,) <$%> ddw w0 <*%> ddw w1 <*%> ddw w2) `break`
		(pressQ w0 `first` pressQ w1 `first` pressQ w2)
	pure (w0, w1, w2)

pressQ :: WindowId -> React s (Singleton KeyDown) ()
pressQ wid = bool (pressQ wid) (pure ()) . isQ =<< keyDown wid

isQ :: KeySym -> Bool
isQ Xk_q = True
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
