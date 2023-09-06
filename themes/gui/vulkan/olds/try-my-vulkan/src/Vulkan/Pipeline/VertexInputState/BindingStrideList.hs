{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.BindingStrideList where

import GHC.Generics
import Foreign.Storable.SizeAlignment
import Data.Kind

import Vulkan.Pipeline.VertexInputState.Flatten

class BindingStrideList a k v where
	bindingStrideList :: [(SizeAlignment, v)]

	default bindingStrideList ::
		BindingStrideListList (Flatten (Rep a)) k v => [(SizeAlignment, v)]
	bindingStrideList = bindingStrideListList @(Flatten (Rep a)) @k @v

instance (BindingStrideListList (Flatten (Rep a)) k v) => BindingStrideList a k v

class BindingStrideListList (ts :: [Type]) k v where
	bindingStrideListList :: [(SizeAlignment, v)]

instance BindingStrideListList '[] k v where bindingStrideListList = []

instance (SizeAlignmentList t, BindingStrideListList ts k v, TypeVal a v) =>
	BindingStrideListList (AddType [t] (a :: k) ': ts) k v where
	bindingStrideListList =
		(wholeSizeAlignment @t, typeVal @k @a @v) : bindingStrideListList @ts @k @v

newtype AddType v t = AT v deriving Show

type family SubType t where SubType (AddType v t) = v

type family MapSubType t where
	MapSubType '[] = '[]
	MapSubType (AddType v t ': ats) = v ': MapSubType ats

class TypeVal (t :: k) v where typeVal :: v

instance TypeVal 'False Int where typeVal = 123
instance TypeVal 'True Int where typeVal = 321
