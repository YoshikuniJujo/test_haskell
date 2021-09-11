{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.DefaultWindow (
	K.KeyEv,
	K.KeyDown, keyDown,
	K.KeyUp, keyUp,
	) where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set
import Data.KeySym

import qualified Control.Moffy.Event.Key as K

keyDown :: React s (LoadDefaultWindow :- K.KeyDown :- 'Nil) KeySym
keyDown = adjust . K.keyDown =<< adjust loadDefaultWindow

keyUp :: React s (LoadDefaultWindow :- K.KeyUp :- 'Nil) KeySym
keyUp = adjust . K.keyUp =<< adjust loadDefaultWindow
