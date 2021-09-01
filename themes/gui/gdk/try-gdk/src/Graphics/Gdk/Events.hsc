{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Events (
	-- * GET EVENT
	gdkWithEvent, gdkEventsPending,

	-- * DEBUG
	gdkGetShowEvents, gdkSetShowEvents

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.Concurrent
import Data.Int
import System.GLib.Bool

import Graphics.Gdk.EventStructures.Internal

#include <gdk/gdk.h>

gdkEventsPending :: IO Bool
gdkEventsPending = gbooleanToBool <$> c_gdk_events_pending

foreign import ccall "gdk_events_pending"
	c_gdk_events_pending :: IO #type gboolean

gdkWithEvent :: (forall s . Maybe (GdkEvent s) -> IO a) -> IO a
gdkWithEvent f = c_gdk_event_get >>= \case
	NullPtr -> f Nothing
	p -> (f . Just . GdkEvent =<< newForeignPtr p (pure ()))
		<* c_gdk_event_free p

foreign import ccall "gdk_event_get" c_gdk_event_get :: IO (Ptr GdkEventTag)

gdkGetShowEvents :: IO Bool
gdkGetShowEvents = gbooleanToBool <$> c_gdk_get_show_events

foreign import ccall "gdk_get_show_events"
	c_gdk_get_show_events :: IO #type gboolean

gdkSetShowEvents :: Bool -> IO ()
gdkSetShowEvents = c_gdk_set_show_events . boolToGboolean

foreign import ccall "gdk_set_show_events"
	c_gdk_set_show_events :: #{type gboolean} -> IO ()
