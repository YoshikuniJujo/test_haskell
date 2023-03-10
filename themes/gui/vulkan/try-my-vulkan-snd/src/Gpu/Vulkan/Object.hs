{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Object where

import GHC.TypeNats
import Foreign.Ptr
import Data.Kind.Object qualified as K
import Data.Proxy
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

data Object = Static K.Object | Dynamic Nat K.Object

type List algn t nm = Static (K.List algn t nm)
type Atom algn t mnm = Static (K.Atom algn t mnm)
type ObjImage algn t nm = Static (K.ObjImage algn t nm)

type DynList n algn t nm = Dynamic n (K.List algn t nm)

data ObjectLength obj where
	ObjectLengthStatic :: K.ObjectLength kobj -> ObjectLength ('Static kobj)
	ObjectLengthDynamic ::
		K.ObjectLength kobj -> ObjectLength ('Dynamic n kobj)

deriving instance Eq (ObjectLength obj)
deriving instance Show (ObjectLength obj)

pattern ObjectLengthImage kr kw kh kd <- (ObjectLengthStatic (K.ObjectLengthImage kr kw kh kd))
	where ObjectLengthImage kr kw kh kd = ObjectLengthStatic (K.ObjectLengthImage kr kw kh kd)

pattern ObjectLengthAtom :: ObjectLength ('Static (K.Atom algn t nm))
pattern ObjectLengthAtom <- ObjectLengthStatic K.ObjectLengthAtom where
	ObjectLengthAtom = ObjectLengthStatic K.ObjectLengthAtom

pattern ObjectLengthList :: Int -> ObjectLength ('Static (K.List algn t nm))
pattern ObjectLengthList n <- ObjectLengthStatic (K.ObjectLengthList n) where
	ObjectLengthList n = ObjectLengthStatic (K.ObjectLengthList n)

pattern ObjectLengthDynList :: Int -> ObjectLength ('Dynamic n (K.List algn t nm))
pattern ObjectLengthDynList n <- ObjectLengthDynamic (K.ObjectLengthList n) where
	ObjectLengthDynList n = ObjectLengthDynamic (K.ObjectLengthList n)

type family ObjectType obj where
	ObjectType (Static kobj) = K.ObjectType kobj
	ObjectType (Dynamic n kobj) = K.ObjectType kobj

class SizeAlignment obj where
	objectSize :: ObjectLength obj -> Int
	objectSize' :: ObjectLength obj -> Int
	objectAlignment :: Int

instance K.SizeAlignment kobj => SizeAlignment (Static kobj) where
	objectSize (ObjectLengthStatic kln) = K.objectSize kln
	objectSize' = objectSize
	objectAlignment = K.objectAlignment @kobj

instance (KnownNat n, K.SizeAlignment kobj) =>
	SizeAlignment (Dynamic n kobj) where
	objectSize (ObjectLengthDynamic kln) = K.objectSize kln
	objectSize' obj = fromIntegral (natVal (Proxy :: Proxy n)) * objectSize obj
	objectAlignment = K.objectAlignment @kobj

class WholeSize objs where
	wholeSize :: Int -> HeteroParList.PL ObjectLength objs -> Int

instance WholeSize '[] where wholeSize sz _ = sz

instance (SizeAlignment obj, WholeSize objs) => WholeSize (obj ': objs) where
	wholeSize sz (ln :** lns) =
		wholeSize (((sz - 1) `div` algn + 1) * algn + objectSize' ln) lns
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

class Offset (obj :: Object) objs where
	offset :: Int -> HeteroParList.PL ObjectLength objs -> Int
	range :: HeteroParList.PL ObjectLength objs -> Int

instance SizeAlignment obj => Offset obj (obj ': objs) where
	offset ofst _ = ((ofst - 1) `div` algn + 1) * algn
		where algn = objectAlignment @obj
	range (ln :** _) = objectSize ln

instance {-# OVERLAPPABLE #-} (SizeAlignment obj', Offset obj objs) =>
	Offset obj (obj' ': objs) where
	offset ofst (ln :** lns) = offset @obj
		(((ofst - 1) `div` algn + 1) * algn + objectSize' ln) lns
		where algn = objectAlignment @obj'
	range (_ :** lns) = range @obj lns
