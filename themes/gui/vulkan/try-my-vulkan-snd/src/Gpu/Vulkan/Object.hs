{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object where

import GHC.TypeNats
import Data.Kind.Object qualified as K
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

data Object = Static K.Object | Dynamic Nat K.Object

data ObjectLength obj where
	ObjectLengthStatic :: K.ObjectLength kobj -> ObjectLength ('Static kobj)
	ObjectLengthDynamic ::
		K.ObjectLength kobj -> ObjectLength ('Dynamic n kobj)

type family ObjectType obj where
	ObjectType (Static kobj) = K.ObjectType kobj
	ObjectType (Dynamic n kobj) = K.ObjectType kobj

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	objectAlignment :: Int

instance K.SizeAlignment kobj => SizeAlignment (Static kobj) where
	objectSize (ObjectLengthStatic kln) = K.objectSize kln
	objectAlignment = K.objectAlignment @kobj

instance (KnownNat n, K.SizeAlignment kobj) =>
	SizeAlignment (Dynamic n kobj) where
	objectSize (ObjectLengthDynamic kln) =
		fromIntegral (natVal (Proxy :: Proxy n)) * K.objectSize kln
	objectAlignment = K.objectAlignment @kobj

class WholeSize objs where
	wholeSize :: Int -> HeteroParList.PL ObjectLength objs -> Int

instance WholeSize '[] where wholeSize sz _ = sz

instance (SizeAlignment obj, WholeSize objs) => WholeSize (obj ': objs) where
	wholeSize sz (ln :** lns) =
		wholeSize (((sz - 1) `div` algn + 1) * algn + objectSize ln) lns
		where algn = objectAlignment @obj
