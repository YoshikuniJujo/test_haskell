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

foreign import capi "cglm/cglm.h glm_rotate" c_glm_rotate ::
	Ptr () -> #{type float} -> Ptr #{type float} -> IO ()

glmLookat ::
	[#{type float}] -> [#{type float}] -> [#{type float}] -> [#{type float}]
glmLookat eye center up = unsafePerformIO . ($ pure) $ runContT do
	peye <- ContT $ allocaArray 3
	pcenter <- ContT $ allocaArray 3
	pup <- ContT $ allocaArray 3
	pdest <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	lift do	pokeArray peye eye
		pokeArray pcenter center
		pokeArray pup up
		c_glm_lookat peye pcenter pup $ castPtr pdest
		peekArray 16 pdest

foreign import capi "cglm/cglm.h glm_lookat" c_glm_lookat ::
	Ptr #{type float} -> Ptr #{type float} -> Ptr #{type float} ->
	Ptr () -> IO ()

glmPerspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} ->
	[#{type float}]
glmPerspective fovy aspect nearZ farZ = unsafePerformIO . ($ pure) $ runContT do
	pdest <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	lift do	c_glm_perspective fovy aspect nearZ farZ $ castPtr pdest
		peekArray 16 pdest

foreign import capi "cglm/cglm.h glm_perspective" c_glm_perspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} ->
	Ptr () -> IO ()
