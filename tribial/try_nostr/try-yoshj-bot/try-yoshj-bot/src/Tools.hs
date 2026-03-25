{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (

	toHex,
	withFileCreationMask, mask

	) where

import Control.Exception (bracket)
import Data.Bits
import Data.Char
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as T
import System.Posix.Types
import System.Posix.Files
import Numeric

toHex :: BSC.ByteString -> T.Text
toHex = T.pack . strToHexStr . BSC.unpack

strToHexStr :: String -> String
strToHexStr = concat . (sh <$>) . map ord
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

withFileCreationMask :: FileMode -> IO a -> IO a
withFileCreationMask md =
	bracket (setFileCreationMask md) setFileCreationMask . const

mask :: FileMode
mask = groupReadWriteMode .|. otherReadWriteMode

groupReadWriteMode, otherReadWriteMode :: FileMode
groupReadWriteMode = groupReadMode .|. groupWriteMode
otherReadWriteMode = otherReadMode .|. otherWriteMode
