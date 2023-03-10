{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object where

import GHC.TypeNats
import Foreign.Ptr
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

nextObject :: forall kobj . K.SizeAlignment kobj =>
	Ptr (K.ObjectType kobj) -> K.ObjectLength kobj -> Ptr (K.ObjectType kobj)
nextObject p ln = p `plusPtr` n
	where
	n = ((K.objectSize ln - 1) `div` algn + 1) * algn
	algn = K.objectAlignment @kobj

class StoreObject v obj where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v
	objectLength :: v -> ObjectLength obj

instance K.StoreObject v kobj => StoreObject v (Static kobj) where
	storeObject p (ObjectLengthStatic kln) = K.storeObject p kln
	loadObject p (ObjectLengthStatic kln) = K.loadObject p kln
	objectLength = ObjectLengthStatic . K.objectLength

instance (K.SizeAlignment kobj, K.StoreObject v kobj, KnownNat n) =>
	StoreObject [v] (Dynamic n kobj) where
	storeObject p0 (ObjectLengthDynamic kln) =
		go p0 (natVal (Proxy :: Proxy n))
		where
		go _ _ [] = pure ()
		go _ n _ | n < 1 = pure ()
		go p n (x : xs) =
			K.storeObject p kln x >> go (nextObject p kln) (n - 1) xs
	loadObject p0 (ObjectLengthDynamic kln) = go p0 (natVal (Proxy :: Proxy n))
		where
		go _ n | n < 1 = pure []
		go p n = (:) <$> K.loadObject p kln <*> go (nextObject p kln) (n - 1)
	objectLength = ObjectLengthDynamic . K.objectLength . head
