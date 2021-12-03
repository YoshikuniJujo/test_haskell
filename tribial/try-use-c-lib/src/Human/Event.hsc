{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.Event where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Exception
import Data.Word
import Data.Char
import System.IO
import System.IO.Unsafe

import qualified Data.ByteString as BS

#include <human.h>

data Event s = Event (Ptr (Event s)) deriving Show

withEvent :: (forall s . Event s -> IO a) -> IO a
withEvent f = bracket
	(c_hm_get_event =<< c_wrap_get_char dummyGetCChar)
	c_hm_event_destroy $ f . Event

foreign import ccall "hm_get_event" c_hm_get_event ::
	FunPtr (IO CChar) -> IO (Ptr (Event s))

dummyGetCChar :: IO CChar
dummyGetCChar = pure . fromIntegral $ ord 'x'

hGetCChar :: Handle -> IO CChar
hGetCChar h = (`BS.useAsCString` peek) =<< BS.hGet h 1

foreign import ccall "hm_event_destroy"
	c_hm_event_destroy :: Ptr (Event s) -> IO ()

foreign import ccall "wrapper"
	c_wrap_get_char :: IO CChar -> IO (FunPtr (IO CChar))

enum "EventType" ''#{type HmEventType} [''Show, ''Storable] [
	("EventTypeTick", #{const HM_EVENT_TYPE_TICK})
	]

eventType :: Event s -> EventType
eventType (Event pev) = unsafePerformIO $ #{peek HmEventAny, event_type} pev

noFinalizer :: Ptr a -> ForeignPtr a
noFinalizer = unsafePerformIO . (`newForeignPtr` pure ())

newtype Sealed s a = Sealed a deriving Show

struct "EventTick" #{size HmEventTick}
	[	("times", ''CInt,
			[| #{peek HmEventTick, times} |],
			[| #{poke HmEventTick, times} |]) ]
	[''Show]

getEventTick :: Event s -> (EventType, Sealed s EventTick)
getEventTick ev@(Event pev) =
	(eventType ev, Sealed . EventTick_ . noFinalizer $ castPtr pev)

pattern EventEventTick :: Sealed s EventTick -> Event s
pattern EventEventTick evt <- (getEventTick -> (EventTypeTick, evt))
