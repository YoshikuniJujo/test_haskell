{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key (
	-- * Type
	KeyEv,
	-- * Key Down Event
	KeyDown, pattern OccKeyDown, keyDown,
	-- * Key Up Event
	KeyUp, pattern OccKeyUp, keyUp,
	-- * Key
	Key(..), pattern AsciiKey,
	module Control.Moffy.Event.Key.Internal.XK ) where

import Control.Moffy (React, Request(..), await)
import Control.Moffy.Event.Key.Internal (Key(..))
import Control.Moffy.Event.Key.Internal.XK
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-))
import Data.Bool (bool)
import Data.Char (chr)

---------------------------------------------------------------------------

-- * EVENT
-- * PATTERN

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

data KeyDown = KeyDownReq deriving (Show, Eq, Ord)
numbered 9 [t| KeyDown |]
instance Request KeyDown where
	data Occurred KeyDown = OccKeyDown Key deriving (Show, Eq, Ord)

keyDown :: React s (Singleton KeyDown) Key
keyDown = await KeyDownReq \(OccKeyDown k) -> k

data KeyUp = KeyUpReq deriving (Show, Eq, Ord)
numbered 9 [t| KeyUp |]
instance Request KeyUp where
	data Occurred KeyUp = OccKeyUp Key deriving (Show, Eq, Ord)

keyUp :: React s (Singleton KeyUp) Key
keyUp = await KeyUpReq \(OccKeyUp k) -> k

type KeyEv = KeyDown :- KeyUp :- 'Nil

---------------------------------------------------------------------------
-- PATTERN
---------------------------------------------------------------------------

pattern AsciiKey :: Char -> Key
pattern AsciiKey c <- (asciiKey -> Just c)

asciiKey :: Key -> Maybe Char
asciiKey (Key k) =
	bool Nothing (Just . chr $ fromIntegral k) (0x20 <= k && k <= 0x7e)
