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

---------------------------------------------------------------------------
-- DELETE EVENT
---------------------------------------------------------------------------

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent WindowId deriving Show

deleteEvent :: React s (Singleton DeleteEvent) WindowId
deleteEvent = await DeleteEventReq \(OccDeleteEvent wid) -> wid
