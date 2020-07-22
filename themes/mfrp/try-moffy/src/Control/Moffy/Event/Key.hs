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
	module Control.Moffy.Event.Key.XK ) where

import Control.Moffy
import Control.Moffy.Event.Key.XK
import Control.Moffy.Event.Key.Internal
import Data.Type.Set
import Data.Char

pattern AsciiKey :: Char -> Key
pattern AsciiKey c <- (toAsciiChar -> Just c)

toAsciiChar :: Key -> Maybe Char
toAsciiChar (Key k)
	| 0x20 <= k && k <= 0x7e = Just . chr $ fromIntegral k
	| otherwise = Nothing

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
