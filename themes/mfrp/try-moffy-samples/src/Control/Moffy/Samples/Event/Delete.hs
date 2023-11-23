{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Event.Delete where

import Control.Moffy
import Data.Type.Set (numbered, Singleton)

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent deriving Show

deleteEvent :: React s (Singleton DeleteEvent) ()
deleteEvent = await DeleteEventReq \OccDeleteEvent -> ()
