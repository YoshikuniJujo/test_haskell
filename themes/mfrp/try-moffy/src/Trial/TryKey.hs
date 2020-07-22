{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryKey where

import Prelude hiding (repeat, break)

import Control.Moffy
import Control.Moffy.Run
import Control.Moffy.Handle (Handle', retry)
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key
import Control.Moffy.Handle.XField
import Control.Moffy.Handle.XField.Key
import Data.Type.Set
import Data.OneOrMore
import Data.Or

import Field

action :: Sig s (DeleteEvent :- KeyEv) (Or Char Char) ()
action = () <$ repeat (asciiKey `first` asciiKeyUp) `break` deleteEvent

asciiKey :: React s KeyEv Char
asciiKey = adjust keyDown >>= \case
	AsciiKey c -> pure c
	XK_Return -> pure '\n'
	_ -> asciiKey

asciiKeyUp :: React s KeyEv Char
asciiKeyUp = adjust keyUp >>= \case
	AsciiKey c -> pure c
	XK_Return -> pure '\n'
	_ -> asciiKeyUp

run :: IO ()
run = do
	f <- openField "TRY KEY" [exposureMask, keyPressMask, keyReleaseMask]
	interpret (retry $ handleKey f) print action
	closeField f

handleKey :: Field -> Handle' IO (DeleteEvent :- KeyEv)
handleKey = handleXField (\case KeyEv kev -> Just $ expand kev; _ -> Nothing) Nothing
