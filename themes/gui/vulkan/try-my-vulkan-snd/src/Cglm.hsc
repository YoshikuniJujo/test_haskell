{-# LANGUAGE DataKinds #-}
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
