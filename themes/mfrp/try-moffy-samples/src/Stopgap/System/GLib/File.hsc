{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.File where

import Foreign.Ptr
import Stopgap.Data.Ptr

data FTag

newtype F = F (Ptr FTag) deriving Show

instance IsPtr F where type Tag F = FTag; fromPtr = F; toPtr (F p) = p

-- foreign import ccall "g_file_load_contents" c_g_file_load_contents ::
--	Ptr FTag -> 
