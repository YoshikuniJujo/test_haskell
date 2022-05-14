{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Cglm (
	module Cglm, C.Vec2(..), C.Vec3(..), C.Vec4, C.Mat4
	) where

import Data.Foldable
import Data.Traversable.Utils
import Data.Either
import Data.List.Length

import qualified Cglm.Core as C

#include <cglm/cglm.h>

glmRotate :: C.Mat4 -> #{type float} -> C.Vec3 -> C.Mat4
glmRotate m angle (C.Vec3 axis) = listVec4ToMat4
	$ C.glmRotate (mat4ToListVec4 m) angle (toList axis)

glmLookat :: C.Vec3 -> C.Vec3 -> C.Vec3 -> C.Mat4
glmLookat (C.Vec3 eye) (C.Vec3 center) (C.Vec3 up) = listToMat4
	$ C.glmLookat (toList eye) (toList center) (toList up)

glmPerspective ::
	#{type float} -> #{type float} -> #{type float} -> #{type float} -> C.Mat4
glmPerspective fovy aspect nearZ farZ =
	listToMat4 $ C.glmPerspective fovy aspect nearZ farZ

modifyMat4 :: Int -> Int -> (#{type float} -> #{type float}) -> C.Mat4 -> C.Mat4
modifyMat4 i j f (C.Mat4 m) = C.Mat4 $ modifyElem2 i j f m

mat4ToList :: C.Mat4 -> [#{type float}]
mat4ToList (C.Mat4 m) = concat $ toList <$> toList m

mat4ToListVec4 :: C.Mat4 -> [C.Vec4]
mat4ToListVec4 (C.Mat4 m) = C.Vec4 <$> toList m

listToMat4 :: [#{type float}] -> C.Mat4
listToMat4 = C.Mat4 . unsafeToLength . (unsafeToLength <$>) . separateN 4

listVec4ToMat4 :: [C.Vec4] -> C.Mat4
listVec4ToMat4 = C.vec4ToMat4 . unsafeToLength

unsafeToLength :: ListToLengthL n => [a] -> LengthL n a
unsafeToLength = fst . fromRight (error "bad") . splitL

sampleMat4 :: C.Mat4
sampleMat4 = C.Mat4 $
	(1 :. 0 :. 0 :. 0 :. NilL) :.
	(0 :. 1 :. 0 :. 0 :. NilL) :.
	(0 :. 0 :. 1 :. 0 :. NilL) :.
	(0 :. 0 :. 0 :. 1 :. NilL) :. NilL

separateN :: Int -> [a] -> [[a]]
separateN _ [] = []
separateN n xs = take n xs : separateN n (drop n xs)
