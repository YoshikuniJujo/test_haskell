{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Key (
	-- * Key Event
	KeyEv,
	-- * Key Down Event
	KeyDown, pattern OccKeyDown, keyDown,
	-- * Key Up Event
	KeyUp, pattern OccKeyUp, keyUp
	) where

import Control.Moffy (React, Request(..), await)
import Control.Moffy.Event.Window
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-))
import Data.Bool (bool)
import Data.KeySym

---------------------------------------------------------------------------

-- * EVENT
--	+ KEY DOWN
--	+ KEY UP
--	+ KEY EVENT
-- * PATTERN

---------------------------------------------------------------------------
-- EVENT
---------------------------------------------------------------------------

-- KEY DOWN

data KeyDown = KeyDownReq deriving (Show, Eq, Ord)
numbered [t| KeyDown |]
instance Request KeyDown where
	data Occurred KeyDown = OccKeyDown WindowId KeySym deriving Show

keyDown :: WindowId -> React s (Singleton KeyDown) KeySym
keyDown wid0 = maybe (keyDown wid0) pure =<<
	await KeyDownReq \(OccKeyDown wid k) -> bool Nothing (Just k) $ wid == wid0

-- KEY UP

data KeyUp = KeyUpReq deriving (Show, Eq, Ord)
numbered [t| KeyUp |]
instance Request KeyUp where data Occurred KeyUp = OccKeyUp WindowId KeySym deriving Show

keyUp :: WindowId -> React s (Singleton KeyUp) KeySym
keyUp wid0 = maybe (keyUp wid0) pure =<<
	await KeyUpReq \(OccKeyUp wid k) -> bool Nothing (Just k) $ wid == wid0

-- KEY EVENT

type KeyEv = KeyDown :- KeyUp :- 'Nil
