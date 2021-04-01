{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib (cairoCopyPath) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Control.Monad.Primitive
import Data.CairoContext

import CairoPathT

cairoCopyPath :: PrimMonad m => CairoT (PrimState m) -> m CairoPathT
cairoCopyPath (CairoT fcr) = unsafeIOToPrim
	$ withForeignPtr fcr \pcr -> mkCairoPathT =<< c_cairo_copy_path pcr

foreign import ccall "cairo_copy_path" c_cairo_copy_path ::
	Ptr (CairoT s) -> IO (Ptr CairoPathT)
