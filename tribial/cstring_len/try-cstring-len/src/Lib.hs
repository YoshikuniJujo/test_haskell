{-# LANGUAGE LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

sample :: IO CString
sample = newCString "Hello, world!"

clength :: CString -> IO Int
clength p = (0 /=) <$> peek p >>=
	\case False -> pure 0; True -> (1 +) <$> clength (p `plusPtr` 1)

cStringToCStringLen :: CString -> IO CStringLen
cStringToCStringLen cs = (cs ,) <$> clength cs
