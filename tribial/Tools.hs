{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools (toByteString) where

import Data.List
import Data.Tree
import System.IO
import System.IO.Temp
import System.Environment (getArgs)
import System.Directory
import Foreign.Ptr
import Foreign.Marshal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

toByteString :: Ptr a -> Int -> IO BS.ByteString
toByteString p n = BSI.create n $ \b -> copyBytes b (castPtr p) n
