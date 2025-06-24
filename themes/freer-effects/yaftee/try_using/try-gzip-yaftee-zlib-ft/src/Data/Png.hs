{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png where

import Data.ByteString.FingerTree qualified as BSF

fileHeader :: BSF.ByteString
fileHeader = "\x89PNG\r\n\SUB\n"
