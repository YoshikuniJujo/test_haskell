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

	create, recreate, I, Binded, CreateInfo(..),

	-- * GET MEMORY REQUIREMENTS

	getMemoryRequirements, getMemoryRequirementsBinded,

	-- * MEMORY BARRIER

	MemoryBarrier(..), SubresourceRange(..), MemoryBarrierListToMiddle,

	-- * BLIT

	Blit(..), SubresourceLayers(..)

	) where

import Gpu.Vulkan.Image.Internal