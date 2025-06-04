{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png where

import Data.ByteString.Lazy qualified as LBS

fileHeader :: LBS.ByteString
fileHeader = "\x89PNG\r\n\SUB\n"
