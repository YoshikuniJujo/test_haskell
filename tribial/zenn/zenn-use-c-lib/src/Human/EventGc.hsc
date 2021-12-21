{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.EventGc where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word
import System.IO.Unsafe

#include <human.h>

data Event = Event (ForeignPtr Event) deriving Show

foreign import ccall "hm_get_event_only_tick" c_hm_get_event :: IO (Ptr Event)
foreign import ccall "hm_event_destroy" c_hm_event_destroy :: Ptr Event -> IO ()

getEventGc :: IO Event
getEventGc = Event <$> do
	pe <- c_hm_get_event
	newForeignPtr pe $ c_hm_event_destroy pe

enum "EventType" ''#{type HmEventType} [''Show, ''Storable] [
	("EventTypeTick", #{const HM_EVENT_TYPE_TICK}) ]

struct "EventTick" #{size HmEventTick}
	[	("eventType", ''(), [| const $ pure () |],
			[| \p _ -> #{poke HmEventTick, event_type}
				p EventTypeTick |]),
		("times", ''CInt, [| #{peek HmEventTick, times} |],
			[| #{poke HmEventTick, times} |]) ]
	[''Show]

getEventType :: Event -> EventType
getEventType (Event fev) =
	unsafePerformIO $ withForeignPtr fev #{peek HmEventAny, event_type}

getEventTick :: Event -> (EventType, EventTick)
getEventTick ev@(Event fev) = (getEventType ev, EventTick_ $ castForeignPtr fev)

pattern EventEventTickGc :: EventTick -> Event
pattern EventEventTickGc evt <- (getEventTick -> (EventTypeTick, evt)) where
	EventEventTickGc (EventTick_ fev) = Event $ castForeignPtr fev
