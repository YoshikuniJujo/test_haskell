{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Kind.Object where

import Foreign.Storable
import Data.Kind
import Data.HeteroList

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

deriving instance Show (ObjectLength obj)

class Offset (obj :: Object) objs where
	offset :: Int -> HeteroVarList ObjectLength objs -> Int
	range :: HeteroVarList ObjectLength objs -> Int

instance Storable (ObjectType obj) => Offset obj (obj ': objs) where
	offset ofst _ = ((ofst - 1) `div` algn + 1) * algn
		where algn = alignment @(ObjectType obj) undefined
	range (ln :...: _) = objectSize ln

instance (Storable (ObjectType obj'), Offset obj objs) => Offset obj (obj' ': objs) where
	offset ofst (ln :...: lns) =
		offset @obj (((ofst - 1) `div` algn + 1) * algn + objectSize ln) lns
		where algn = alignment @(ObjectType obj') undefined
	range (_ :...: lns) = range @obj lns
