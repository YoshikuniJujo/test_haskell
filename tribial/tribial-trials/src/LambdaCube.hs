{-# LANGUAGE ScopedTypeVariables, TypeApplications, RankNTypes #-}
{-# LANGUAGE KindSignatures, DataKinds, ConstraintKinds, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes, InstanceSigs #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
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

class MapTypeVal (c :: Type -> Constraint) (as :: [Type]) where
	mapTypeVal :: (forall a . c a => b) -> [b]

{-
instance (MapTypeVal2 c as) => MapTypeVal c as where
	mapTypeVal x = mapTypeVal2 @c @as y
		where
		y _ = x
		-}

-- class MapTypeVal c (as :: [Type]) where mapTypeVal :: (forall a . b) -> [b]
instance MapTypeVal c '[] where mapTypeVal _ = []

sampleTypeNames2 :: [String]
sampleTypeNames2 = mapTypeVal2 @TypeName @'[Int, Double] tn
	where
	tn :: forall a . TypeName a => a -> String
	tn _ = typeName @a

typeNameList :: forall as . MapTypeVal2 TypeName as => [String]
typeNameList = mapTypeVal2 @TypeName @as tn
	where
	tn :: forall a . TypeName a => a -> String
	tn _ = typeName @a

{-
sampleTypeNames3 :: [String]
sampleTypeNames3 = mapTypeVal @TypeName @'[Int, Double] (tn undefined)
	where
	tn :: forall a . TypeName a => a -> String
	tn _ = typeName @a
	-}

instance (c a, MapTypeVal2 c as) => MapTypeVal c (a ': as) where
-- instance (MapTypeVal c as) => MapTypeVal c (a ': as) where
--	mapTypeVal :: forall a' b . (c a' => b) -> [b]
--	mapTypeVal x = y @a undefined : mapTypeVal @c @as (y undefined)
	mapTypeVal x = x @a : mapTypeVal2 @c @as y
		where
--		y :: forall a b . c a => a -> b
		y _ = x @a

--

class TypeName2 a where typeName2 :: a -> String
instance TypeName2 Int where typeName2 _ = "Int"
instance TypeName2 Double where typeName2 _ = "Double"
instance TypeName2 Bool where typeName2 _ = "Bool"

class MapTypeVal2 c (as :: [Type]) where mapTypeVal2 :: (forall a . c a => a -> b) -> [b]
instance MapTypeVal2 c '[] where mapTypeVal2 _ = []

instance (c a, MapTypeVal2 c as) => MapTypeVal2 c (a ': as) where
--	mapTypeVal2 x = x (undefined :: a) : mapTypeVal2 @c @as x
	mapTypeVal2 x = x @a undefined : mapTypeVal2 @c @as x

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

typeSizes :: forall (as :: [Type]) . MapTypeVal2 Storable as => [Int]
typeSizes = mapTypeVal2 @Storable @as sizeOf

typeSizeAlignments :: forall (as :: [Type]) . MapTypeVal2 Storable as => [(Int, Int)]
typeSizeAlignments = mapTypeVal2 @Storable @as (\x -> (sizeOf x, alignment x))
