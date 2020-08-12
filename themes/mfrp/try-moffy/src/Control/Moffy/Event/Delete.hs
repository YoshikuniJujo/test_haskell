{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Delete (
	-- * Type
	DeleteEvent, pattern OccDeleteEvent,

	-- * Event
	-- ** Delete Event
	deleteEvent ) where

import Control.Moffy (React, Request(..), await)
import Data.Type.Set (numbered, Singleton)

---------------------------------------------------------------------------

---------------------------------------------------------------------------
-- DELETE EVENT
---------------------------------------------------------------------------

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent deriving (Show, Eq, Ord)

deleteEvent :: React s (Singleton DeleteEvent) ()
deleteEvent = await DeleteEventReq \OccDeleteEvent -> ()
