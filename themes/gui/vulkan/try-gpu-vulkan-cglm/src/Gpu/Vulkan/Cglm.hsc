{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Cglm (

	-- * TRANSFORM

	translate, scale, rotate, perspective, lookat, C.rad,

	-- * VECTOR

	C.Vec2(..), C.Vec3(..), C.Vec4(..),

	-- * MATRIX

	C.Mat4(..), mat4Identity, modifyMat4, mat4Mul, C.mat4ToVec4s

	) where

import Data.Foldable
import Data.Traversable.Utils
import Data.Either
import Data.List.Length

import qualified Gpu.Vulkan.Cglm.Core as C

#include <cglm/cglm.h>

rotate :: C.Mat4 -> #{type float} -> C.Vec3 -> C.Mat4
rotate m angle (C.Vec3 axis) = listVec4ToMat4
	$ C.glmRotate (mat4ToListVec4 m) angle (toList axis)

lookat :: C.Vec3 -> C.Vec3 -> C.Vec3 -> C.Mat4
lookat (C.Vec3 eye) (C.Vec3 center) (C.Vec3 up) = listVec4ToMat4
	$ C.glmLookat (toList eye) (toList center) (toList up)

perspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} -> C.Mat4
perspective fovy aspect nearZ farZ =
	listVec4ToMat4 $ C.glmPerspective fovy aspect nearZ farZ

mat4Identity :: C.Mat4
mat4Identity = listVec4ToMat4 C.glmMat4Identity

mat4Mul :: C.Mat4 -> C.Mat4 -> C.Mat4
mat4Mul m1 m2 =
	listVec4ToMat4 $ C.glmMat4Mul (mat4ToListVec4 m1) (mat4ToListVec4 m2)

modifyMat4 :: Int -> Int -> (#{type float} -> #{type float}) -> C.Mat4 -> C.Mat4
modifyMat4 i j f (C.Mat4 m) = C.Mat4 $ modifyElem2 i j f m

_mat4ToList :: C.Mat4 -> [#{type float}]
_mat4ToList (C.Mat4 m) = concat $ toList <$> toList m

mat4ToListVec4 :: C.Mat4 -> [C.Vec4]
mat4ToListVec4 (C.Mat4 m) = C.Vec4 <$> toList m

_listToMat4 :: [#{type float}] -> C.Mat4
_listToMat4 = C.Mat4 . unsafeToLength . (unsafeToLength <$>) . separateN 4
	where
	separateN :: Int -> [a] -> [[a]]
	separateN _ [] = []
	separateN n xs = take n xs : separateN n (drop n xs)

listVec4ToMat4 :: [C.Vec4] -> C.Mat4
listVec4ToMat4 = C.vec4ToMat4 . unsafeToLength

unsafeToLength :: ListToLengthL n => [a] -> LengthL n a
unsafeToLength = fst . fromRight (error "bad") . splitL

translate :: C.Mat4 -> C.Vec3 -> C.Mat4
translate m v = listVec4ToMat4
	$ C.glmTranslate (mat4ToListVec4 m) (vec3ToList v)

_listToVec3 :: [#{type float}] -> C.Vec3
_listToVec3 = C.Vec3 . unsafeToLength

vec3ToList :: C.Vec3 -> [#{type float}]
vec3ToList (C.Vec3 fs) = toList fs

scale :: C.Mat4 -> C.Vec3 -> C.Mat4
scale m v = listVec4ToMat4
	$ C.glmScale (mat4ToListVec4 m) (vec3ToList v)
