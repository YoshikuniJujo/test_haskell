{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Cmd.TypeLevel where

import Data.Kind
import Data.TypeLevel.Tuple.MapIndex qualified as TMapIndex
import Data.Word

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout

firstSet' :: forall
	(dss :: [(Type, (Type, [DescriptorSetLayout.BindingType]))])
	(pl :: [(Type, [DescriptorSetLayout.BindingType])]) .
	SetPos (TMapIndex.M1_2 dss) pl => Word32
firstSet' = firstSet @(TMapIndex.M1_2 dss) @pl

class SetPos
	(dss :: [(Type, [DescriptorSetLayout.BindingType])])
	(pl :: [(Type, [DescriptorSetLayout.BindingType])]) where
	firstSet :: Word32

class IsPrefixOf
	(dss :: [(Type, [DescriptorSetLayout.BindingType])])
	(pl :: [(Type, [DescriptorSetLayout.BindingType])])

instance IsPrefixOf '[] t
instance {-# OVERLAPPABLE #-} IsPrefixOf ts ts' => IsPrefixOf (t ': ts) (t ': ts')

instance IsPrefixOf ts ts' => SetPos (t ': ts) (t ': ts') where firstSet = 0
instance {-# OVERLAPPABLE #-} SetPos ts ts' => SetPos (t : ts) ts' where
	firstSet = firstSet @ts @ts' + 1
