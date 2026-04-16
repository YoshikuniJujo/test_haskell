{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Keyboard where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Concurrent
import Control.Concurrent.STM
import Data.Bits
import Data.Function
import Data.Word
import System.IO
import System.Clock

keyboard :: FilePath
keyboard = "/dev/snd/midiC1D0"

getKeyboardHandle :: IO Handle
getKeyboardHandle = openFile keyboard ReadMode

withKeyboard :: IO (TChan NoteEvent)
withKeyboard = do
	c <- atomically newTChan
	h <- getKeyboardHandle
	(print =<<) . forkIO $ allocaBytes 3 \p -> fix \go -> do
		t <- getTime Monotonic
		3 <- hGetBuf h p 3
		[st, n, v] <- peekArray 3 p
		atomically $ writeTChan c (t, readNote st n v)
		go
	pure c

type NoteEvent = (TimeSpec, Note)

data Note
	= Note Word8 Word8 Word8
	| NoNote Word8 Word8 Word8
	deriving (Show, Eq)

readNote :: Word8 -> Word8 -> Word8 -> Note
readNote st n v
	| st `shiftR` 4 == 9 = Note (st .&. 0xf) n v
	| otherwise = NoNote st n v
