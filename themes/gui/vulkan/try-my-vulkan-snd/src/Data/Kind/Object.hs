{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Kind.Object where

import Foreign.Ptr
import Foreign.Marshal.Array
import Foreign.Storable
import Data.Kind
import Data.MonoTraversable
import Data.HeteroList

import qualified Data.Sequences as Seq

data Object = Atom Type | List Type

type family ObjectType obj where
	ObjectType ('Atom t) = t
	ObjectType ('List t) = t

data ObjectLength (obj :: Object) where
	ObjectLengthAtom :: ObjectLength ('Atom t)
	ObjectLengthList :: Int -> ObjectLength ('List t)

objectSize :: forall obj . Storable (ObjectType obj) => ObjectLength obj -> Int
objectSize ObjectLengthAtom = sizeOf @(ObjectType obj) undefined
objectSize (ObjectLengthList n) = n * ((sz - 1) `div` algn + 1) * algn
	where 
	sz = sizeOf @(ObjectType obj) undefined
	algn = alignment @(ObjectType obj) undefined

objectAlignment :: forall obj . Storable (ObjectType obj) => Int
objectAlignment = alignment @(ObjectType obj) undefined

deriving instance Show (ObjectLength obj)

class Offset (obj :: Object) objs where
	offset :: Int -> HeteroVarList ObjectLength objs -> Int
	range :: HeteroVarList ObjectLength objs -> Int

instance Storable (ObjectType obj) => Offset obj (obj ': objs) where
	offset ofst _ = ((ofst - 1) `div` algn + 1) * algn
		where algn = alignment @(ObjectType obj) undefined
	range (ln :...: _) = objectSize ln

instance {-# OVERLAPPABLE #-} (Storable (ObjectType obj'), Offset obj objs) =>
	Offset obj (obj' ': objs) where
	offset ofst (ln :...: lns) = offset @obj
		(((ofst - 1) `div` algn + 1) * algn + objectSize ln) lns
		where algn = alignment @(ObjectType obj') undefined
	range (_ :...: lns) = range @obj lns

class WholeSize objs where
	wholeSize :: Int -> HeteroVarList ObjectLength objs -> Int

instance WholeSize '[] where wholeSize sz _ = sz

instance (Storable (ObjectType obj), WholeSize objs) =>
	WholeSize (obj ': objs) where
	wholeSize sz (ln :...: lns) =
		wholeSize (((sz - 1) `div` algn + 1) * algn + objectSize ln) lns
		where
		algn = alignment @(ObjectType obj) undefined

class StoreObject v (obj :: Object) where
	storeObject :: Ptr (ObjectType obj) -> ObjectLength obj -> v -> IO ()
	loadObject :: Ptr (ObjectType obj) -> ObjectLength obj -> IO v

instance Storable t => StoreObject t ('Atom t) where
	storeObject p ObjectLengthAtom x = poke p x
	loadObject p ObjectLengthAtom = peek p

instance (
	MonoFoldable v, Seq.IsSequence v,
	Storable t, Element v ~ t ) => StoreObject v ('List t) where
	storeObject p (ObjectLengthList n) xs = pokeArray p . take n $ otoList xs
	loadObject p (ObjectLengthList n) = Seq.fromList <$> peekArray n p
