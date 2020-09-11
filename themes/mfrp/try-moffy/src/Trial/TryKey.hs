{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryKey (tryKey) where

import Prelude hiding (repeat, break)

import Control.Moffy (Sig, React, adjust, first, repeat, break)
import Control.Moffy.Event.Delete (DeleteEvent, deleteEvent)
import Control.Moffy.Event.Key (
	KeyEv, keyDown, keyUp, pattern AsciiKey, pattern XkReturn )
import Control.Moffy.Handle (retry)
import Control.Moffy.Handle.XField (handle)
import Control.Moffy.Run (interpret)
import Data.Type.Set ((:-))
import Data.Or (Or)

import Field (openField, closeField, exposureMask, keyPressMask, keyReleaseMask)

---------------------------------------------------------------------------

tryKey :: IO ()
tryKey = do
	f <- openField "TRY KEY" [exposureMask, keyPressMask, keyReleaseMask]
	interpret (retry $ handle Nothing f) print keySig <* closeField f

keySig :: Sig s (DeleteEvent :- KeyEv) (Or Char Char) ()
keySig = () <$ repeat (asciiKey `first` asciiKeyUp) `break` deleteEvent

asciiKey :: React s KeyEv Char
asciiKey = adjust keyDown >>= \case
	AsciiKey c -> pure c; XkReturn -> pure '\n'; _ -> asciiKey

asciiKeyUp :: React s KeyEv Char
asciiKeyUp = adjust keyUp >>= \case
	AsciiKey c -> pure c; XkReturn -> pure '\n'; _ -> asciiKeyUp
