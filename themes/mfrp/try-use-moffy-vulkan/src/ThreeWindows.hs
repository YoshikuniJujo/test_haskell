{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ThreeWindows (threeWindows) where

import Prelude hiding (break)

import Data.Type.Set
import Data.Type.Flip
import Data.Bool
import Data.KeySym
import Control.Moffy
import Control.Moffy.Event.Gui
import Control.Moffy.Event.Window
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key

threeWindows :: Sig s GuiEv () ()
threeWindows = do
	w0 <- waitFor $ adjust windowNew
	w1 <- waitFor $ adjust windowNew
	w2 <- waitFor $ adjust windowNew
	_ <- (waitFor . find (\(x, y, z) -> not $ x || y || z) $ (,,) <$%> ddw w0 <*%> ddw w1 <*%> ddw w2) `break`
		(pressQ w0 `first` pressQ w1 `first` pressQ w2)

	pure ()

pressQ :: WindowId -> React s (Singleton KeyDown) ()
pressQ wid = bool (pressQ wid) (pure ()) . isQ =<< keyDown wid

ddw :: WindowId -> Sig s GuiEv Bool ()
ddw i = do
	emit True
	waitFor do
		adjust $ deleteEvent i
		adjust $ windowDestroy i
	emit False
	waitFor never

isQ :: KeySym -> Bool
isQ = \case Xk_q -> True; _ -> False
