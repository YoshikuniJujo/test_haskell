{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryConfigureEvent where

import Prelude hiding (repeat, break)

import Control.Monad

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Handle
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.TChan
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField
import Data.Type.Set
import Data.Type.Flip
import Data.Map (Map, singleton)
import Graphics.Gtk (gtkMainQuit)

import Trial.Draw.Viewable

tryConfigure :: Sig s (LoadDefaultWindow :- WindowConfigure :- 'Nil) [Message] ()
tryConfigure = (: []) . Message . show <$%> repeat (adjust . windowConfigure =<< adjust loadDefaultWindow)

runTryConfigure :: (Monoid a, Adjustable es (DefaultWindowEv :+: GuiEv)) =>
	GtkDrawer' a -> Sig s es (Map WindowId a) r -> IO (r, Maybe WindowId)
runTryConfigure dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	r <- interpretSt (retrySt $ handleDefaultWindow `mergeSt` liftHandle' (handle Nothing cr c)) c' s Nothing
	gtkMainQuit
	pure r

tryConfigureMain :: IO ()
tryConfigureMain = void $ runTryConfigure (\_ _ -> mapM_ putMessage) (prepare $ adjustSig tryConfigure)

prepare :: Sig s (DefaultWindowEv :+: GuiEv) a r -> Sig s (DefaultWindowEv :+: GuiEv) (Map WindowId a) (Either r (Maybe a, ()))
prepare s = do
	i <- waitFor $ adjust windowNew
	waitFor . adjust $ storeDefaultWindow i
	singleton i <$%>
		(adjustSig $ s `break` deleteEvent i)
