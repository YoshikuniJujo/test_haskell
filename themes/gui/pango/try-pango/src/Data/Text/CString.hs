{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Text.CString where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Foreign as T

peekCStringText :: CString -> IO T.Text
peekCStringText = T.peekCStringLen <=< cStringToCStringLen

cStringToCStringLen :: CString -> IO CStringLen
cStringToCStringLen cs = (cs ,) <$> clength cs

clength :: CString -> IO Int
clength p = (0 /=) <$> peek p >>=
	\case False -> pure 0; True -> (1 +) <$> clength (p `plusPtr` 1)
