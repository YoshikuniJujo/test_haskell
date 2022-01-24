{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Pipeline.VertexInputState.GetBindingOffset where

import GHC.Generics
import Foreign.Storable.SizeAlignment
import Control.Arrow
import Data.Kind

import Vulkan.Pipeline.VertexInputState.BindingStrideList (MapSubType)
import Vulkan.Pipeline.VertexInputState.Flatten

class BindingOffset tss t where
	bindingOffset :: Maybe (Int, Offset)

	default bindingOffset ::
		BindingOffsetList (MapSubType (Flatten (Rep tss))) t => Maybe (Int, Offset)
	bindingOffset = bindingOffsetList @(MapSubType (Flatten (Rep tss))) @t

instance BindingOffsetList (MapSubType (Flatten (Rep tss))) t =>
	BindingOffset tss t

class BindingOffsetList (tss :: [Type]) t where
	bindingOffsetList :: Maybe (Int, Offset)

instance BindingOffsetList '[] t where bindingOffsetList = Nothing

instance (SizeAlignmentListUntil t ts, BindingOffsetList tss t) =>
	BindingOffsetList ([ts] ': tss) t where
	bindingOffsetList = case offsetOf @t @ts of
		Nothing -> ((+ 1) `first`) <$> bindingOffsetList @tss @t
		Just os -> Just (0, os)
