{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (withFileCreationMask, mask) where

import Control.Exception (bracket)
import Data.Bits
import System.Posix.Types
import System.Posix.Files

withFileCreationMask :: FileMode -> IO a -> IO a
withFileCreationMask md =
	bracket (setFileCreationMask md) setFileCreationMask . const

mask :: FileMode
mask = groupReadWriteMode .|. otherReadWriteMode

groupReadWriteMode, otherReadWriteMode :: FileMode
groupReadWriteMode = groupReadMode .|. groupWriteMode
otherReadWriteMode = otherReadMode .|. otherWriteMode
