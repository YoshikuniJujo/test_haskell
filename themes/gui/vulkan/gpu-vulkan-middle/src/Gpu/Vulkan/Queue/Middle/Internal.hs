{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Middle.Internal (

	-- * SUBMIT AND WAIT IDLE

	submit, waitIdle, Q(..)

	) where

import Foreign.Marshal.Array
import Data.HeteroParList qualified as HeteroParList

import Gpu.Vulkan.Middle
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum
import Gpu.Vulkan.Fence.Middle.Internal qualified as Fence.M
import Gpu.Vulkan.Queue.Core qualified as C

newtype Q = Q C.Q deriving Show

submit :: SubmitInfoListToCore ns =>
	Q -> HeteroParList.PL SubmitInfo ns -> Maybe Fence.M.F -> IO ()
submit (Q q) sis mf = submitInfoListToCore sis \csis ->
	let sic = length csis in allocaArray sic \psis -> do
		pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis
			$ Fence.M.maybeFToCore mf
		throwUnlessSuccess $ Result r

waitIdle :: Q -> IO ()
waitIdle (Q q) = throwUnlessSuccess . Result =<< C.waitIdle q
