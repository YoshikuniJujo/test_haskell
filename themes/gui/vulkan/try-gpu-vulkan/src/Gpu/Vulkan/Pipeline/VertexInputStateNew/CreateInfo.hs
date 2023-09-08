{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Pipeline.VertexInputStateNew.CreateInfo where

import GHC.TypeNats
import Data.Kind
import Data.TypeLevel.Maybe qualified as TMaybe
import Data.Default
import Data.Bits

import qualified Gpu.Vulkan.Pipeline.VertexInputState.Middle as M
import qualified Gpu.Vulkan.VertexInput.Internal as VtxInp

-- CREATE INFO

data CreateInfo mn (vibs :: [(Type, VtxInp.Rate)]) (vias :: [(Nat, Type)]) =
	CreateInfo {
		createInfoNext :: TMaybe.M mn,
		createInfoFlags :: M.CreateFlags }

deriving instance Show (TMaybe.M mn) => Show (CreateInfo mn vibs vias)

instance Default (CreateInfo 'Nothing vibs vias) where
	def = CreateInfo {
		createInfoNext = TMaybe.N, createInfoFlags = zeroBits }
