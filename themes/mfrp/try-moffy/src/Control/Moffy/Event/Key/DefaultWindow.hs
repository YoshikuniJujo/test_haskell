{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.DefaultWindow (
	K.KeyEv,
	K.KeyDown, keyDown,
	K.KeyUp, keyUp,
	K.Key(..), pattern K.AsciiKey,
	module Control.Moffy.Event.Key.Internal.TryKeyValue ) where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set

import Control.Moffy.Event.Key.Internal.TryKeyValue

import qualified Control.Moffy.Event.Key as K

keyDown :: React s (LoadDefaultWindow :- K.KeyDown :- 'Nil) K.Key
keyDown = adjust . K.keyDown =<< adjust loadDefaultWindow

keyUp :: React s (LoadDefaultWindow :- K.KeyUp :- 'Nil) K.Key
keyUp = adjust . K.keyUp =<< adjust loadDefaultWindow
