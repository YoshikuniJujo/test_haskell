{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cglm.Core where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Monad.Cont
import System.IO.Unsafe

#include <cglm/cglm.h>

glmRotate :: [#{type float}] -> #{type float} -> [#{type float}] ->
	[#{type float}]
glmRotate m angle axis = unsafePerformIO . ($ pure) $ runContT do
	pm <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	paxis <- ContT $ allocaArray 3
	lift do	pokeArray pm m
		pokeArray paxis axis
		print pm
		c_glm_rotate (castPtr pm) angle paxis
		peekArray 16 pm

foreign import capi "cglm/affine.h glm_rotate" c_glm_rotate ::
	Ptr () -> #{type float} -> Ptr #{type float} -> IO ()
