{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.DefaultWindow (
	DefaultWindowEv,
	StoreDefaultWindow(..), pattern OccStoreDefaultWindow, storeDefaultWindow,
	LoadDefaultWindow(..), pattern OccLoadDefaultWindow, loadDefaultWindow
	) where

import Control.Moffy
import Control.Moffy.Event.Window
import Data.Type.Set
import Data.Bool

newtype StoreDefaultWindow = StoreDefaultWindowReq WindowId deriving (Show, Eq, Ord)
numbered [t| StoreDefaultWindow |]
instance Request StoreDefaultWindow where
	data Occurred StoreDefaultWindow = OccStoreDefaultWindow WindowId deriving Show

storeDefaultWindow :: WindowId -> React s (Singleton StoreDefaultWindow) ()
storeDefaultWindow wid = bool (storeDefaultWindow wid) (pure ())
	=<< await (StoreDefaultWindowReq wid) \(OccStoreDefaultWindow wid') -> wid == wid'

data LoadDefaultWindow = LoadDefaultWindowReq deriving (Show, Eq, Ord)
numbered [t| LoadDefaultWindow |]
instance Request LoadDefaultWindow where
	data Occurred LoadDefaultWindow = OccLoadDefaultWindow WindowId deriving Show

loadDefaultWindow :: React s (Singleton LoadDefaultWindow) WindowId
loadDefaultWindow = await LoadDefaultWindowReq \(OccLoadDefaultWindow wid) -> wid

type DefaultWindowEv = StoreDefaultWindow :- LoadDefaultWindow :- 'Nil
