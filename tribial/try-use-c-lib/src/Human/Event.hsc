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

-- EVENT

data Event s = Event (Ptr (Event s)) deriving Show

withEvent :: TChan CChar -> (forall s . Event s -> IO a) -> IO a
withEvent ch f = bracket (c_hm_get_event =<< wrap_getCChar getc)
	c_hm_event_destroy (f . Event)
	where
	getc = atomically $ bool (readTChan ch) (pure 0) =<< isEmptyTChan ch

foreign import ccall "hm_get_event"
	c_hm_get_event :: FunPtr (IO CChar) -> IO (Ptr (Event s))

foreign import ccall "hm_event_destroy"
	c_hm_event_destroy :: Ptr (Event s) -> IO ()

foreign import ccall "wrapper"
	wrap_getCChar :: IO CChar -> IO (FunPtr (IO CChar))

-- EVENT TYPE

enum "EventType" ''#{type HmEventType} [''Show, ''Storable] [
	("EventTypeTick", #{const HM_EVENT_TYPE_TICK}),
	("EventTypeChar", #{const HM_EVENT_TYPE_CHAR})
	]

eventType :: Event s -> EventType
eventType (Event pev) = unsafePerformIO $ #{peek HmEventAny, event_type} pev

newtype Sealed s a = Sealed a deriving Show

noFinalizer :: Ptr a -> ForeignPtr a
noFinalizer = unsafePerformIO . (`newForeignPtr` pure ())

-- EVENT TICK

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

eventTickToTimes :: Sealed s EventTick -> CInt
eventTickToTimes (Sealed evt) = eventTickTimes evt

-- EVENT CHAR

struct "EventChar" #{size HmEventChar}
	[	("character", ''CChar,
			[| #{peek HmEventChar, character} |],
			[| #{poke HmEventChar, character} |]) ]
	[''Show]

getEventChar :: Event s -> (EventType, Sealed s EventChar)
getEventChar ev@(Event pev) =
	(eventType ev, Sealed . EventChar_ . noFinalizer $ castPtr pev)

pattern EventEventChar :: Sealed s EventChar -> Event s
pattern EventEventChar evc <- (getEventChar -> (EventTypeChar, evc))

eventCharToCharacter :: Sealed s EventChar -> CChar
eventCharToCharacter (Sealed evc) = eventCharCharacter evc

-- READ HANDLE

hGetAndPushCChar :: Handle -> IO (TChan CChar)
hGetAndPushCChar h = do
	ch <- atomically newTChan
	_ <- forkIO $ forever do
		cs <- getCCharList
		(atomically . writeTChan ch) `mapM_` cs
	pure ch
	where
	getCCharList = allocaBytes 64 \cstr -> do
		cnt <- hGetBufSome h cstr 64
		peekArray cnt cstr
