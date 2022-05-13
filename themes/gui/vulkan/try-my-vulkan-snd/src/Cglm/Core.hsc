{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cglm.Core where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Cont
import Data.Foldable
import Data.Either
import Data.List.Length
import System.IO.Unsafe

#include <cglm/cglm.h>

newtype {-# CTYPE "cglm/cglm.h" "vec4" #-}
	Vec4 = Vec4 (LengthL 4 #{type float}) deriving Show

instance Storable Vec4 where
	sizeOf _ = #{size vec4}
	alignment _ = #{alignment vec4}
	peek p = Vec4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (Vec4 v) = pokeArray (castPtr p) $ toList v

glmRotate :: [Vec4] -> #{type float} -> [#{type float}] -> [Vec4]
glmRotate m angle axis = unsafePerformIO . ($ pure) $ runContT do
	pm <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	paxis <- ContT $ allocaArray 3
	lift do	pokeArray pm m
		pokeArray paxis axis
		print pm
		c_glm_rotate pm angle paxis
		peekArray 4 pm

foreign import capi "cglm/cglm.h glm_rotate" c_glm_rotate ::
	Ptr Vec4 -> #{type float} -> Ptr #{type float} -> IO ()

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
