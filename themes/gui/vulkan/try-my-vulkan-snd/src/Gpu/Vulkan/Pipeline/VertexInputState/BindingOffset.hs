{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputState.BindingOffset where

import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Foreign.Storable.SizeAlignment
import Control.Arrow
import Data.Kind

class BindingOffset (tss :: [(Type, k)]) t where bindingOffset :: Maybe (Int, Offset)

instance BindingOffsetNoType (TMapIndex.M0_2 tss) t => BindingOffset tss t where
	bindingOffset = bindingOffsetNoType @(TMapIndex.M0_2 tss) @t

class BindingOffsetNoType (tss :: [Type]) t where
	bindingOffsetNoType :: Maybe (Int, Offset)

instance BindingOffsetNoType '[] t where bindingOffsetNoType = Nothing

instance (SizeAlignmentListUntil t ts, BindingOffsetNoType tss t) =>
	BindingOffsetNoType (ts ': tss) t where
	bindingOffsetNoType = case offsetOf @t @ts of
		Nothing -> ((+ 1) `first`) <$> bindingOffsetNoType @tss @t
		Just os -> Just (0, os)
