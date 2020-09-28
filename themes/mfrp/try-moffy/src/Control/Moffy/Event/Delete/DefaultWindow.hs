{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Delete.DefaultWindow (
	D.DeleteEvent, deleteEvent
	) where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set

import qualified Control.Moffy.Event.Delete as D

deleteEvent :: React s (LoadDefaultWindow :- D.DeleteEvent :- 'Nil) ()
deleteEvent = adjust . D.deleteEvent =<< adjust loadDefaultWindow
