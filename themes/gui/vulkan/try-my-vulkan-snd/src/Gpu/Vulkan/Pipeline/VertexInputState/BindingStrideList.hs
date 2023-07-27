{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.BindingStrideList (
	BindingStrideList(..), TypeVal(..) ) where

import Foreign.Storable.SizeAlignment
import Data.Kind

class BindingStrideList k (ts :: [(Type, k)]) v where
	bindingStrideList :: [(SizeAlignment, v)]

instance BindingStrideList k '[] v where bindingStrideList = []

instance (SizeAlignmentList t, BindingStrideList k ts v, TypeVal a v) =>
	BindingStrideList k ('(t, (a :: k)) ': ts) v where
	bindingStrideList = (wholeSizeAlignment @t, typeVal @k @a @v) :
		bindingStrideList @k @ts @v

class TypeVal (t :: k) v where typeVal :: v
