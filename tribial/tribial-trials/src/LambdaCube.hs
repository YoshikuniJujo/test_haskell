{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LambdaCube where

import Foreign.Storable
import Data.Kind

class TypeName a where typeName :: String

instance TypeName Int where typeName = "Int"
instance TypeName Double where typeName = "Double"
instance TypeName Bool where typeName = "Bool"

typeNameDouble :: String
typeNameDouble = typeName @Double

class MapTypeVal c (as :: [Type]) where mapTypeVal :: (forall a . c a => b) -> [b]
instance MapTypeVal c '[] where mapTypeVal _ = []

{-
instance (c a, MapTypeVal c as) => MapTypeVal c (a ': as) where
	mapTypeVal :: (forall a . c a => b) -> [b]
	mapTypeVal x = x @a : mapTypeVal @c @as x
	-}

--

class TypeName2 a where typeName2 :: a -> String
instance TypeName2 Int where typeName2 _ = "Int"
instance TypeName2 Double where typeName2 _ = "Double"
instance TypeName2 Bool where typeName2 _ = "Bool"

class MapTypeVal2 c (as :: [Type]) where mapTypeVal2 :: (forall a . c a => a -> b) -> [b]
instance MapTypeVal2 c '[] where mapTypeVal2 _ = []

instance (c a, MapTypeVal2 c as) => MapTypeVal2 c (a ': as) where
	mapTypeVal2 x = x (undefined :: a) : mapTypeVal2 @c @as x

typeName' :: forall a . TypeName a => a -> String
typeName' _ = typeName @a

{-
convert :: forall c a b . c a => (forall a . c a => b) -> a -> b
convert tn _ = tn @a
-}

sampleTypeNames :: [String]
sampleTypeNames = mapTypeVal2 @TypeName @[Int, Double, Bool] typeName'

sampleTypeSizes :: [Int]
sampleTypeSizes = mapTypeVal2 @Storable @[Int, Double, Bool, ()] sizeOf
