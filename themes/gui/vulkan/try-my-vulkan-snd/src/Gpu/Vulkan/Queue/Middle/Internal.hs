{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Middle.Internal where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Kind
import Data.HeteroList hiding (length)

import Gpu.Vulkan
import Gpu.Vulkan.Exception.Middle.Internal
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Fence.Type as Fence
import qualified Gpu.Vulkan.Fence.Middle.Internal as Fence.M

import qualified Gpu.Vulkan.Middle.Internal as M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Queue.Core as C

newtype Q = Q C.Q deriving Show

submit :: SubmitInfoListToCore nssssvsss =>
	Q -> HeteroVarList (V4 SubmitInfo) nssssvsss -> Maybe (Fence.F sf) -> IO ()
submit (Q q) sis mf = ($ pure) $ runContT do
	csis <- submitInfoListToCore sis
	let	sic = length csis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis . Fence.M.maybeFToCore $ (\(Fence.F f) -> f) <$> mf
		throwUnlessSuccess $ Result r

class SubmitInfoListToCore (nssssvsss :: [(Type, [Type], [(Type, [Type])], [Type])]) where
	submitInfoListToCore ::
		HeteroVarList (V4 SubmitInfo) nssssvsss ->
		ContT r IO [C.SubmitInfo]

instance SubmitInfoListToCore '[] where
	submitInfoListToCore HVNil = pure []

instance (
	Pointable n, CommandBufferListToMiddle svss,
	SubmitInfoListToCore nssssvsss ) =>
	SubmitInfoListToCore ('(n, sss, svss, ssss) ': nssssvsss) where
	submitInfoListToCore (V4 si :...: sis) = do
		csi <- M.submitInfoToCore $ submitInfoToMiddle si
		csis <- submitInfoListToCore sis
		pure $ csi : csis

waitIdle :: Q -> IO ()
waitIdle (Q q) = throwUnlessSuccess . Result =<< C.waitIdle q
