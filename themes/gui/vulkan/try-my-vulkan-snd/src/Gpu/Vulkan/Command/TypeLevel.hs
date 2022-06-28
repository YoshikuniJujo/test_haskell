{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Command.TypeLevel where

import Data.Kind
import Data.Word

import qualified Gpu.Vulkan.DescriptorSetLayout.Type as DescriptorSetLayout

class SetPos
	(ds :: (Type, [DescriptorSetLayout.BindingType]))
	(pl :: [(Type, [DescriptorSetLayout.BindingType])]) where
	firstSet :: Word32

-- CHECK
-- 	forall s . Foo s -> Bar s -> String
--
-- 	instance Some (Foo a) (Bar a) where ...
-- 	instance {-# OVERLAPPABLE #-} Some (Foo a) (Foo b) where ...
