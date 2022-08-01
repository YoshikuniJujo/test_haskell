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
import Control.Arrow
import Control.Monad.Cont
import Data.Kind
import Data.HeteroList hiding (length)

import Gpu.Vulkan
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import {-# SOURCE #-} qualified Gpu.Vulkan.Fence.Middle as Fence

import qualified Gpu.Vulkan.Middle as M
import qualified Gpu.Vulkan.Core as C
import qualified Gpu.Vulkan.Queue.Core as C

newtype Q = Q C.Q deriving Show

submitNewNew :: SubmitInfoListToCoreNew nssssvsss =>
	Q -> HeteroVarList (V4 SubmitInfoNew) nssssvsss -> Maybe Fence.F -> IO ()
submitNewNew (Q q) sis mf = ($ pure) $ runContT do
	csis <- submitInfoListToCoreNew sis
	let	sic = length csis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis $ Fence.maybeFToCore mf
		throwUnlessSuccess $ Result r

submitNew :: SubmitInfoListToCore nssssvss =>
	Q -> HeteroVarList (V4 SubmitInfo) nssssvss -> Maybe Fence.F -> IO ()
submitNew (Q q) sis mf = ($ pure) $ runContT do
	csis <- submitInfoListToCore sis
	let	sic = length csis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis $ Fence.maybeFToCore mf
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

class SubmitInfoListToCore (nssssvss :: [(Type, [Type], Type, [Type])]) where
	submitInfoListToCore ::
		HeteroVarList (V4 SubmitInfo) nssssvss ->
		ContT r IO [C.SubmitInfo]

instance SubmitInfoListToCore '[] where submitInfoListToCore HVNil = pure []

instance (Pointable n, SubmitInfoListToCore nssssvss) =>
	SubmitInfoListToCore ('(n, sss, s, vs) ': nssssvss) where
	submitInfoListToCore (V4 si :...: sis) = do
		csi <- M.submitInfoToCore $ submitInfoToMiddle si
		csis <- submitInfoListToCore sis
		pure $ csi : csis

submit :: Pointable n => Q -> [SubmitInfo n sss s vs] -> Maybe Fence.F -> IO ()
submit (Q q)
	(length &&& id -> (sic, sis)) f = ($ pure) $ runContT do
	csis <- (M.submitInfoToCore . submitInfoToMiddle) `mapM` sis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis
			$ Fence.maybeFToCore f
		throwUnlessSuccess $ Result r

submit' :: forall n sss vs . (Pointable n, SemaphorePipelineStageFlagsFromMiddle sss) =>
	Q -> [M.SubmitInfo n vs] -> Maybe Fence.F -> IO ()
submit' q = submit @_ @sss q . (submitInfoFromMiddle @sss <$>)

waitIdle :: Q -> IO ()
waitIdle (Q q) = throwUnlessSuccess . Result =<< C.waitIdle q
