{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryKey (tryKey, tryKeyMain) where

import Prelude hiding (repeat, break)

import Control.Moffy (Sig, React, adjust, adjustSig, waitFor, first, repeat, break, Adjustable)
import Control.Moffy.Event.Delete (DeleteEvent, deleteEvent)
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Key.DefaultWindow (KeyEv, keyDown, keyUp)
import Control.Moffy.Handle (retrySt, beforeSt, liftHandle')
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretSt, interpretReactSt)
import Data.Type.Set ((:-))
import Data.Or (Or)
import Data.KeySym

import Field (openField, closeField, exposureMask, keyPressMask, keyReleaseMask)

import Control.Monad
import Control.Moffy.Run.GtkField (GuiEv, runGtkMain)
import Data.Map (Map, singleton)
import Data.Type.Set ((:+:))
import Trial.Draw.Viewable (putMessage)
import Graphics.Gtk (gtkMainQuit)
import Control.Moffy.Handle (mergeSt)
import qualified Control.Moffy.Run.TChan as T (interpretSt)
import qualified Control.Moffy.Handle.TChan as T (handle)
import Data.Type.Flip ((<$%>))

---------------------------------------------------------------------------

tryKey :: IO ((), Maybe WindowId)
tryKey = do
	f <- openField "TRY KEY" [exposureMask, keyPressMask, keyReleaseMask]
	interpretSt (retrySt $ handleDefaultWindow `beforeSt` liftHandle' (handle Nothing f)) print keySig Nothing <* closeField f

runTryKey :: (Monoid a, Show a, Adjustable es (DefaultWindowEv :+: GuiEv)) =>
	Sig s es (Map WindowId a) r -> IO (r, Maybe WindowId)
runTryKey s = do
	([], (cr, c, c')) <- runGtkMain (\_ _ x -> print x) []
	r <- T.interpretSt (retrySt $ handleDefaultWindow `beforeSt` liftHandle' (T.handle Nothing cr c)) c' s Nothing
--	r <- interpretSt (retrySt $ handleDefaultWindow `mergeSt` liftHandle' (T.handle Nothing cr c)) print s Nothing
	gtkMainQuit
	pure r

tryKeyMain :: IO ()
tryKeyMain = void $ runTryKey keySig

keySig :: Sig s (WindowNew :- DefaultWindowEv :+: DeleteEvent :- KeyEv) (Map WindowId [Or Char Char]) ()
keySig = () <$ do
	i <- waitFor $ adjust windowNew
	waitFor . adjust $ storeDefaultWindow i
	singleton i . (: []) <$%> adjustSig (repeat (asciiKey `first` asciiKeyUp) `break` deleteEvent i)

asciiKey :: React s (LoadDefaultWindow :- KeyEv) Char
asciiKey = adjust keyDown >>= \case
	Xk_Return -> pure '\n'; c -> pure 'x'

asciiKeyUp :: React s (LoadDefaultWindow :- KeyEv) Char
asciiKeyUp = adjust keyUp >>= \case
	; Xk_Return -> pure '\n'; c -> pure 'x'
