{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.Event where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Bool
import Data.Word
import System.IO
import System.IO.Unsafe

#include <human.h>

data Event s = Event (Ptr (Event s)) deriving Show

foreign import ccall "hm_get_event_only_tick"
	c_hm_get_event_only_tick :: IO (Ptr (Event s))

foreign import ccall "hm_event_destroy"
	c_hm_event_destroy :: Ptr (Event s) -> IO ()

withEventOnlyTick :: (forall s . Event s -> IO a) -> IO a
withEventOnlyTick f =
	bracket c_hm_get_event_only_tick c_hm_event_destroy (f . Event)

enum "EventType" ''#{type HmEventType} [''Show, ''Storable] [
	("EventTypeTick", #{const HM_EVENT_TYPE_TICK}),
	("EventTypeChar", #{const HM_EVENT_TYPE_CHAR}) ]

eventType :: Event s -> EventType
eventType (Event pev) = unsafePerformIO $ #{peek HmEventAny, event_type} pev

struct "EventTick" #{size HmEventTick}
	[	("eventType", ''(), [| const $ pure () |],
			[| \p _ -> #{poke HmEventTick, event_type}
				p EventTypeTick |]),
		("times", ''CInt, [| #{peek HmEventTick, times} |],
			[| #{poke HmEventTick, times} |]) ]
	[''Show]

newtype Sealed s a = Sealed a deriving Show

getEventTick :: Event s -> (EventType, Sealed s EventTick)
getEventTick ev@(Event pev) =
	(eventType ev, Sealed . EventTick_ . noFinalizer $ castPtr pev)

noFinalizer :: Ptr a -> ForeignPtr a
noFinalizer = unsafePerformIO . (`newForeignPtr` pure ())

pattern EventEventTick :: Sealed s EventTick -> Event s
pattern EventEventTick evt <- (getEventTick -> (EventTypeTick, evt))

eventTickToTimes :: Sealed s EventTick -> CInt
eventTickToTimes (Sealed evt) = eventTickTimes evt
