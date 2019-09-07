{-# LANGUAGE CApiFFI, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Base (FileDescriptor(..), c_errno) where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types

newtype FileDescriptor = FileDescriptor CInt deriving (Show, Storable)

foreign import capi "value errno" c_errno :: CInt
