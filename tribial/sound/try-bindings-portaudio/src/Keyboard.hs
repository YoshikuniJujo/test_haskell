{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Keyboard where

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
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
		1 <- hGetBuf h p 1
		t <- getTime Monotonic
		st <- peek p
		if st == 248 || st == 254 then pure () else do
			2 <- hGetBuf h p 2
			[n, v] <- peekArray 2 p
			atomically $ writeTChan c (t, readNote st n v)
		go
	pure c

type NoteEvent = (TimeSpec, Note)

data Note
	= Note Word8 Word8 Word8
	| NoNote Word8 Word8 Word8
	deriving (Show, Eq, Read)

readNote :: Word8 -> Word8 -> Word8 -> Note
readNote st n v
	| st `shiftR` 4 == 9 = Note (st .&. 0xf) n v
	| otherwise = NoNote st n v

textToNoteEvent :: String -> NoteEvent
textToNoteEvent = read

readNoteEvent :: FilePath -> IO [NoteEvent]
readNoteEvent fp = (textToNoteEvent <$>) . lines <$> readFile fp
