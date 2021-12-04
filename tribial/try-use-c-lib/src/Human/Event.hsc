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
import Data.Char
import System.IO
import System.IO.Unsafe

#include <human.h>

data Event s = Event (Ptr (Event s)) deriving Show

withEvent :: TChan CChar -> (forall s . Event s -> IO a) -> IO a
withEvent ch f = bracket
	(c_hm_get_event =<< c_wrap_get_char (getCChar ch))
	c_hm_event_destroy $ f . Event

foreign import ccall "hm_get_event" c_hm_get_event ::
	FunPtr (IO CChar) -> IO (Ptr (Event s))

hGetCCharList :: Handle -> IO [CChar]
hGetCCharList h = allocaBytes 64 \cstr -> do
	cnt <- hGetBufSome h cstr 64
	peekArray cnt cstr

hGetAndPushCChar :: Handle -> IO (TChan CChar)
hGetAndPushCChar h = do
	ch <- atomically newTChan
	_ <- forkIO $ forever do
		cs <- hGetCCharList h
		(atomically . writeTChan ch) `mapM_` cs
	pure ch

getCChar :: TChan CChar -> IO CChar
getCChar ch = atomically
	$ bool (readTChan ch) (pure 0) =<< isEmptyTChan ch

dummyGetCChar :: IO CChar
dummyGetCChar = pure . fromIntegral $ ord 'x'

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
