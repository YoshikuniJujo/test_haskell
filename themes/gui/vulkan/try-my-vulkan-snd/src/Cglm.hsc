{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cglm where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Foldable
import Data.Traversable.Utils
import Data.Either
import Data.List.Length

import qualified Cglm.Core as C

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

glmRotate :: Mat4 -> #{type float} -> Vec3 -> Mat4
glmRotate m angle (Vec3 axis) = listToMat4
	$ C.glmRotate (mat4ToList m) angle (toList axis)

glmLookat :: Vec3 -> Vec3 -> Vec3 -> Mat4
glmLookat (Vec3 eye) (Vec3 center) (Vec3 up) = listToMat4
	$ C.glmLookat (toList eye) (toList center) (toList up)

glmPerspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} -> Mat4
glmPerspective fovy aspect nearZ farZ =
	listToMat4 $ C.glmPerspective fovy aspect nearZ farZ

modifyMat4 :: Int -> Int -> (#{type float} -> #{type float}) -> Mat4 -> Mat4
modifyMat4 i j f (Mat4 m) = Mat4 $ modifyElem2 i j f m

mat4ToList :: Mat4 -> [#{type float}]
mat4ToList (Mat4 m) = concat $ toList <$> toList m

listToMat4 :: [#{type float}] -> Mat4
listToMat4 = Mat4 . unsafeToLength . (unsafeToLength <$>) . separateN 4

unsafeToLength :: ListToLengthL n => [a] -> LengthL n a
unsafeToLength = fst . fromRight (error "bad") . splitL

sampleMat4 :: Mat4
sampleMat4 = Mat4 $
	(1 :. 0 :. 0 :. 0 :. NilL) :.
	(0 :. 1 :. 0 :. 0 :. NilL) :.
	(0 :. 0 :. 1 :. 0 :. NilL) :.
	(0 :. 0 :. 0 :. 1 :. NilL) :. NilL

separateN :: Int -> [a] -> [[a]]
separateN _ [] = []
separateN n xs = take n xs : separateN n (drop n xs)
