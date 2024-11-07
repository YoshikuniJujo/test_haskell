{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Image (

	-- * CREATE

	create, unsafeRecreate, unsafeRecreate', I, Binded, CreateInfo(..),

	-- ** Manage Multiple Image

	Group, group, create', unsafeDestroy, lookup,

	-- * GET MEMORY REQUIREMENTS

	getMemoryRequirements, getMemoryRequirementsBinded,

	-- * MEMORY BARRIER

	MemoryBarrier(..), SubresourceRange(..), MemoryBarrierListToMiddle,

	-- * BLIT

	Blit(..), SubresourceLayers(..),

	-- * ENUM

	module Gpu.Vulkan.Image.Enum

	) where

import Prelude hiding (lookup)
import Gpu.Vulkan.Image.Internal
import Gpu.Vulkan.Image.Enum
