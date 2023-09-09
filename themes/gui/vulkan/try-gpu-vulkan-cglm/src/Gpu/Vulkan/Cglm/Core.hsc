{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Cglm.Core where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Control.Monad.Cont
import Data.Foldable
import Data.Either
import Data.List.Length
import System.IO.Unsafe

import "try-gpu-vulkan" Gpu.Vulkan.Enum qualified as Vk
import Gpu.Vulkan.Pipeline.VertexInputStateNew qualified as Vk.Ppl.VrtxInpSt

#include <cglm/cglm.h>

newtype Vec2 = Vec2 (LengthL 2 #{type float}) deriving (Show, Eq, Ord)
newtype Vec3 = Vec3 (LengthL 3 #{type float}) deriving (Show, Eq, Ord)

instance Storable Vec2 where
	sizeOf _ = #{size vec2}
	alignment _ = #{alignment vec2}
	peek p = Vec2 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 2 (castPtr p)
	poke p (Vec2 v) = pokeArray (castPtr p) $ toList v

instance Storable Vec3 where
	sizeOf _ = #{size vec3}
	alignment _ = #{alignment vec3}
	peek p = Vec3 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 3 (castPtr p)
	poke p (Vec3 v) = pokeArray (castPtr p) $ toList v

instance Vk.Ppl.VrtxInpSt.Formattable Vec2 where
	formatOf = Vk.FormatR32g32Sfloat

instance Vk.Ppl.VrtxInpSt.Formattable Vec3 where
	formatOf = Vk.FormatR32g32b32Sfloat

instance Storable Mat4 where
	sizeOf _ = #{size mat4}
	alignment _ = #{alignment mat4}
	peek p = vec4ToMat4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (mat4ToVec4 -> vs) = pokeArray (castPtr p) $ toList vs

newtype {-# CTYPE "cglm/cglm.h" "vec4" #-}
	Vec4 = Vec4 (LengthL 4 #{type float}) deriving Show

instance Storable Vec4 where
	sizeOf _ = #{size vec4}
	alignment _ = #{alignment vec4}
	peek p = Vec4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (Vec4 v) = pokeArray (castPtr p) $ toList v

newtype {-# CTYPE "cglm/cglm.h" "mat4" #-}
	Mat4 = Mat4 (LengthL 4 (LengthL 4 #{type float})) deriving Show

vec4ToMat4 :: LengthL 4 Vec4 -> Mat4
vec4ToMat4 = Mat4 . ((\(Vec4 l) -> l) <$>)

mat4ToVec4 :: Mat4 -> LengthL 4 Vec4
mat4ToVec4 (Mat4 m) = Vec4 <$> m

glmRotate :: [Vec4] -> #{type float} -> [#{type float}] -> [Vec4]
glmRotate m angle axis = unsafePerformIO . ($ pure) $ runContT do
	pm <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	paxis <- ContT $ allocaArray 3
	lift do	pokeArray pm m
		pokeArray paxis axis
		c_glm_rotate pm angle paxis
		peekArray 4 pm

foreign import capi "cglm/cglm.h glm_rotate" c_glm_rotate ::
	Ptr Vec4 -> #{type float} -> Ptr #{type float} -> IO ()

glmLookat ::
	[#{type float}] -> [#{type float}] -> [#{type float}] -> [Vec4]
glmLookat eye center up = unsafePerformIO . ($ pure) $ runContT do
	peye <- ContT $ allocaArray 3
	pcenter <- ContT $ allocaArray 3
	pup <- ContT $ allocaArray 3
	pdest <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	lift do	pokeArray peye eye
		pokeArray pcenter center
		pokeArray pup up
		c_glm_lookat peye pcenter pup pdest
		peekArray 4 pdest

foreign import capi "cglm/cglm.h glm_lookat" c_glm_lookat ::
	Ptr #{type float} -> Ptr #{type float} -> Ptr #{type float} ->
	Ptr Vec4 -> IO ()

glmPerspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} ->
	[Vec4]
glmPerspective fovy aspect nearZ farZ = unsafePerformIO . ($ pure) $ runContT do
	pdest <- ContT $ allocaBytesAligned (16 * #{size float}) 32
	lift do	c_glm_perspective fovy aspect nearZ farZ pdest
		peekArray 4 pdest

foreign import capi "cglm/cglm.h glm_perspective" c_glm_perspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} ->
	Ptr Vec4 -> IO ()

glmMat4Identity :: [Vec4]
glmMat4Identity = unsafePerformIO . ($ pure) $ runContT do
	pm <- ContT $ allocaArray 4
	lift do	c_glm_mat4_identity pm
		peekArray 4 pm

foreign import capi "cglm/cglm.h glm_mat4_identity" c_glm_mat4_identity ::
	Ptr Vec4 -> IO ()

foreign import capi "cglm/cglm.h glm_rad" rad ::
	#{type float} -> #{type float}

glmMat4Mul :: [Vec4] -> [Vec4] -> [Vec4]
glmMat4Mul m1 m2 = unsafePerformIO . ($ pure) $ runContT do
	pm1 <- ContT $ allocaArray 4
	pm2 <- ContT $ allocaArray 4
	pdest <- ContT $ allocaArray 4
	lift do	pokeArray pm1 m1
		pokeArray pm2 m2
		c_glm_mat4_mul pm1 pm2 pdest
		peekArray 4 pdest

foreign import capi "cglm/cglm.h glm_mat4_mul" c_glm_mat4_mul ::
	Ptr Vec4 -> Ptr Vec4 -> Ptr Vec4 -> IO ()

glmTranslate :: [Vec4] -> [#{type float}] -> [Vec4]
glmTranslate m v = unsafePerformIO . ($ pure) $ runContT do
	pm <- ContT $ allocaArray 4
	pv <- ContT $ allocaArray 3
	lift do	pokeArray pm m
		pokeArray pv v
		c_glm_translate pm pv
		peekArray 4 pm

foreign import capi "cglm/cglm.h glm_translate" c_glm_translate ::
	Ptr Vec4 -> Ptr #{type float} -> IO ()

glmScale :: [Vec4] -> [#{type float}] -> [Vec4]
glmScale m v = unsafePerformIO . ($ pure) $ runContT do
	pm <- ContT $ allocaArray 4
	pv <- ContT $ allocaArray 3
	lift do	pokeArray pm m
		pokeArray pv v
		c_glm_scale pm pv
		peekArray 4 pm

foreign import capi "cglm/cglm.h glm_scale" c_glm_scale ::
	Ptr Vec4 -> Ptr #{type float} -> IO ()
