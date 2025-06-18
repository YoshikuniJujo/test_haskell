{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.FingerTree.Char8 where

import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.FingerTree.Internal

singleton :: Char -> ByteString
singleton = ByteString . Single . BSC.singleton
