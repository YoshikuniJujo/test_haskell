{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cglm where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Foldable
import Data.Either
import Data.List.Length

#include <cglm/cglm.h>

newtype Vec2 = Vec2 (LengthL 2 #{type float}) deriving Show
newtype Vec3 = Vec3 (LengthL 3 #{type float}) deriving Show
newtype Vec4 = Vec4 (LengthL 4 #{type float}) deriving Show

newtype Mat4 = Mat4 (LengthL 4 (LengthL 4 #{type float})) deriving Show

vec4ToMat4 :: LengthL 4 Vec4 -> Mat4
vec4ToMat4 = Mat4 . ((\(Vec4 l) -> l) <$>)

mat4ToVec4 :: Mat4 -> LengthL 4 Vec4
mat4ToVec4 (Mat4 m) = Vec4 <$> m

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

instance Storable Vec4 where
	sizeOf _ = #{size vec4}
	alignment _ = #{alignment vec4}
	peek p = Vec4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (Vec4 v) = pokeArray (castPtr p) $ toList v

instance Storable Mat4 where
	sizeOf _ = #{size mat4}
	alignment _ = #{alignment mat4}
	peek p = vec4ToMat4 . fst . fromRight (error "never occur") . splitL
		<$> peekArray 4 (castPtr p)
	poke p (mat4ToVec4 -> vs) = pokeArray (castPtr p) $ toList vs
