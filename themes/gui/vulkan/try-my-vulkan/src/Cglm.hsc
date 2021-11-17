{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cglm where

import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import Data.Foldable
import Data.Either
import Data.List.Length
import System.IO.Unsafe

#include <cglm/cglm.h>

newtype Vec4 = Vec4 (LengthL 4 #{type float}) deriving Show

newtype Mat4 = Mat4 (LengthL 4 Vec4) deriving Show

vec :: Vec4
vec = Vec4 $ 1 :. 2 :. 3 :. 4 :. NilL

matrix :: Mat4
matrix = Mat4 $
	(Vec4 $ 1 :. 2 :. 3 :. 4 :. NilL) :.
	(Vec4 $ 5 :. 6 :. 7 :. 8 :. NilL) :.
	(Vec4 $ 9 :. 10 :. 11 :. 12 :. NilL) :.
	(Vec4 $ 13 :. 14 :. 15 :. 16 :. NilL) :. NilL

instance Storable Vec4 where
	sizeOf _ = #{size vec4}
	alignment _ = #{alignment vec4}
	peek p = Vec4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (Vec4 v) = pokeArray (castPtr p) $ toList v

instance Storable Mat4 where
	sizeOf _ = #{size mat4}
	alignment _ = #{alignment mat4}
	peek p = Mat4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (Mat4 m) = pokeArray (castPtr p) $ toList m

mat4Mulv :: Mat4 -> Vec4 -> Vec4
mat4Mulv m v = unsafePerformIO $ alloca \pm -> alloca \pv -> alloca \pdst -> do
	poke pm m
	poke pv v
	c_glm_mat4_mulv pm pv pdst
	peek pdst

foreign import capi "cglm/cglm.h glm_mat4_mulv" c_glm_mat4_mulv ::
	Ptr Mat4 -> Ptr Vec4 -> Ptr Vec4 -> IO ()
