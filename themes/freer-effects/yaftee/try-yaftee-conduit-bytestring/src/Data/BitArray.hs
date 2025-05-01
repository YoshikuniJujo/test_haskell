{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.BitArray where

import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS

data B = B { bit0 :: Int, bitsLen :: Int, bitsBody :: BS.ByteString }
	deriving Show

empty :: B
empty = B { bit0 = 0, bitsLen = 0, bitsBody = "" }

data Bit = O | I deriving (Show, Eq, Ord)
