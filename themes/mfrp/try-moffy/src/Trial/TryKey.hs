{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryKey (tryKey) where

import Prelude hiding (repeat, break)

import Control.Moffy (Sig, React, adjust, adjustSig, waitFor, first, repeat, break)
import Control.Moffy.Event.Delete (DeleteEvent, deleteEvent)
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Key.DefaultWindow (
	KeyEv, keyDown, keyUp, pattern AsciiKey, pattern XkReturn )
import Control.Moffy.Handle (retrySt, beforeSt, liftHandle')
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpretSt)
import Data.Type.Set ((:-))
import Data.Or (Or)

import Field (openField, closeField, exposureMask, keyPressMask, keyReleaseMask)

---------------------------------------------------------------------------

tryKey :: IO ((), Maybe WindowId)
tryKey = do
	f <- openField "TRY KEY" [exposureMask, keyPressMask, keyReleaseMask]
	interpretSt (retrySt $ handleDefaultWindow `beforeSt` liftHandle' (handle Nothing f)) print keySig Nothing <* closeField f

keySig :: Sig s (WindowNew :- LoadDefaultWindow :- DeleteEvent :- KeyEv) (Or Char Char) ()
keySig = () <$ do
	i <- waitFor $ adjust windowNew
	adjustSig $ repeat (asciiKey `first` asciiKeyUp) `break` deleteEvent i

asciiKey :: React s (LoadDefaultWindow :- KeyEv) Char
asciiKey = adjust keyDown >>= \case
	AsciiKey c -> pure c; XkReturn -> pure '\n'; _ -> asciiKey

asciiKeyUp :: React s KeyEv Char
asciiKeyUp = adjust keyUp >>= \case
	AsciiKey c -> pure c; XkReturn -> pure '\n'; _ -> asciiKeyUp
