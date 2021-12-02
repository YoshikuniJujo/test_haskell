{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.EventOld where

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

enum "EventType" ''#{type HmEventType} [''Show, ''Storable] [
	("EventTypeTick", #{const HM_EVENT_TYPE_TICK})
	]

struct "EventAny" #{size HmEventAny}
	[	("eventType", ''EventType,
			[| #{peek HmEventAny, event_type} |],
			[| #{poke HmEventAny, event_type} |]) ]
	[''Show]

struct "EventTick" #{size HmEventTick}
	[	("times", ''CInt,
			[| #{peek HmEventTick, times} |],
			[| #{poke HmEventTick, times} |]) ]
	[''Show]

getEvent :: IO Event
getEvent = Event <$> do
	pe <- c_hm_get_event
	newForeignPtr pe $ putStrLn "FINALIZE" >> c_hm_event_free pe

foreign import ccall "hm_get_event" c_hm_get_event :: IO (Ptr Event)
foreign import ccall "hm_event_destroy" c_hm_event_free :: Ptr Event -> IO ()

getEventType :: Event -> EventType
getEventType (Event fev) = unsafePerformIO
	$ withForeignPtr fev #{peek HmEventAny, event_type}

getEventTick :: Event -> (EventType, EventTick)
getEventTick ev@(Event fev) =
	(getEventType ev , EventTick_ $ castForeignPtr fev)

pattern EventEventTick :: EventTick -> Event
pattern EventEventTick evt <- (getEventTick -> (EventTypeTick, evt)) where
	EventEventTick (EventTick_ fev) = Event $ castForeignPtr fev
