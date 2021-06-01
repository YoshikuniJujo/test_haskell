{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.CString (peekCStringText) where

import Foreign.C.String
import Foreign.C.String.Misc
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

peekCStringText :: CString -> IO T.Text
peekCStringText = T.peekCStringLen <=< toCStringLen
