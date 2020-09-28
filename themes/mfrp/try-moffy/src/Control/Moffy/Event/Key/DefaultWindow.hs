{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key.DefaultWindow (
	K.KeyEv,
	K.KeyDown, K.keyDown,
	K.KeyUp, K.keyUp,
	K.Key(..), pattern K.AsciiKey
	) where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow

import qualified Control.Moffy.Event.Key as K
