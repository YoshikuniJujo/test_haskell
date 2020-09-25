{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Delete (
	-- * Type
	DeleteEvent, pattern OccDeleteEvent,
	-- * Event
	deleteEvent ) where

import Control.Moffy (React, Request(..), await)
import Control.Moffy.Event.Window
import Data.Type.Set (numbered, Singleton)
import Data.Bool

---------------------------------------------------------------------------
-- DELETE EVENT
---------------------------------------------------------------------------

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent WindowId deriving Show

deleteEvent :: WindowId -> React s (Singleton DeleteEvent) ()
deleteEvent i0 = bool (deleteEvent i0) (pure ()) =<< await DeleteEventReq \(OccDeleteEvent wid) -> wid == i0
