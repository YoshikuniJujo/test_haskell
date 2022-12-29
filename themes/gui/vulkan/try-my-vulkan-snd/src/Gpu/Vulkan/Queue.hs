{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Monad.Cont
import Data.Kind
import Data.HeteroList hiding (length)

import Gpu.Vulkan
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import qualified Gpu.Vulkan.Fence.Type as Fence
import qualified Gpu.Vulkan.Fence.Middle.Internal as Fence.M

import qualified Gpu.Vulkan.Middle.Internal as M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Queue.Core as C

newtype Q = Q C.Q deriving Show

submitNew :: SubmitInfoListToCoreNew nssssvsss =>
	Q -> HeteroVarList (V4 SubmitInfoNew) nssssvsss -> Maybe (Fence.F sf) -> IO ()
submitNew (Q q) sis mf = ($ pure) $ runContT do
	csis <- submitInfoListToCoreNew sis
	let	sic = length csis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis . Fence.M.maybeFToCore $ (\(Fence.F f) -> f) <$> mf
		throwUnlessSuccess $ Result r

class SubmitInfoListToCoreNew (nssssvsss :: [(Type, [Type], [(Type, [Type])], [Type])]) where
	submitInfoListToCoreNew ::
		HeteroVarList (V4 SubmitInfoNew) nssssvsss ->
		ContT r IO [C.SubmitInfo]

instance SubmitInfoListToCoreNew '[] where
	submitInfoListToCoreNew HVNil = pure []

instance (
	Pointable n, CommandBufferListToMiddle svss,
	SubmitInfoListToCoreNew nssssvsss ) =>
	SubmitInfoListToCoreNew ('(n, sss, svss, ssss) ': nssssvsss) where
	submitInfoListToCoreNew (V4 si :...: sis) = do
		csi <- M.submitInfoToCoreNew $ submitInfoToMiddleNew si
		csis <- submitInfoListToCoreNew sis
		pure $ csi : csis

waitIdle :: Q -> IO ()
waitIdle (Q q) = throwUnlessSuccess . Result =<< C.waitIdle q
