{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Png where

import Data.Sequence qualified as Seq
import Data.Word
import Data.Char

fileHeader :: Seq.Seq Word8
fileHeader = Seq.fromList $ fromIntegral . ord <$> "\x89PNG\r\n\SUB\n"
