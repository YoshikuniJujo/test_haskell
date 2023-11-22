{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Window (
	WindowEv, WindowId(..),
	WindowNew(..), pattern OccWindowNew, windowNew,
	WindowDestroy(..), pattern OccWindowDestroy, windowDestroy,
	WindowConfigure(..), pattern OccWindowConfigure, Configure(..),
	windowConfigure ) where

import Control.Moffy
import Data.Type.Set
import Data.Bool

newtype WindowId = WindowId Integer deriving (Show, Eq, Ord)

data WindowNew = WindowNewReq deriving (Show, Eq, Ord)
numbered [t| WindowNew |]
instance Request WindowNew where
	data Occurred WindowNew = OccWindowNew WindowId deriving Show

windowNew :: React s (Singleton WindowNew) WindowId
windowNew = await WindowNewReq \(OccWindowNew wid) -> wid

data WindowDestroy = WindowDestroyReq WindowId deriving (Show, Eq, Ord)
numbered [t| WindowDestroy |]
instance Request WindowDestroy where
	data Occurred WindowDestroy = OccWindowDestroy WindowId deriving Show

windowDestroy :: WindowId -> React s (Singleton WindowDestroy) ()
windowDestroy i0 =
	bool (windowDestroy i0) (pure ()) =<<
		await (WindowDestroyReq i0) \(OccWindowDestroy i) -> i == i0

data WindowConfigure = WindowConfigureReq deriving (Show, Eq, Ord)
data Configure = Configure {
	windowPosition :: (Integer, Integer),
	windowSize :: (Integer, Integer) } deriving Show
numbered [t| WindowConfigure |]
instance Request WindowConfigure where
	data Occurred WindowConfigure = OccWindowConfigure WindowId Configure

windowConfigure :: WindowId -> React s (Singleton WindowConfigure) Configure
windowConfigure wid0 = maybe (windowConfigure wid0) pure =<<
	await WindowConfigureReq \(OccWindowConfigure wid c) -> bool Nothing (Just c) $ wid == wid0

type WindowEv = WindowNew :- WindowDestroy :- WindowConfigure :- 'Nil
