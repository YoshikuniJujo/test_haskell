{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan (
	module Gpu.Vulkan,
	M.FormatProperties(..)
	) where

import Foreign.Storable.PeekPoke
import Data.Kind
import Data.TypeLevel.Uncurry
import Data.HeteroParList qualified as HeteroParList
import Data.HeteroParList (pattern (:**))

import qualified Gpu.Vulkan.Middle as M
import qualified Gpu.Vulkan.Semaphore as Semaphore
import qualified Gpu.Vulkan.Semaphore.Middle as Semaphore.M
import qualified Gpu.Vulkan.CommandBuffer.Type as CommandBuffer
import qualified Gpu.Vulkan.Pipeline.Enum as Pipeline

data SemaphorePipelineStageFlags ss =
	SemaphorePipelineStageFlags (Semaphore.S ss) Pipeline.StageFlags
	deriving Show

-- deriving instance Show (HeteroParList Semaphore.S ss)

semaphorePipelineStageFlagsToMiddle ::
	HeteroParList.PL SemaphorePipelineStageFlags sss ->
	[(Semaphore.M.S, Pipeline.StageFlags)]
semaphorePipelineStageFlagsToMiddle = HeteroParList.toList
	\(SemaphorePipelineStageFlags (Semaphore.S s) psfs) -> (s, psfs)

data SubmitInfo n sss svss ssss = SubmitInfo {
	submitInfoNext :: Maybe n,
	submitInfoWaitSemaphoreDstStageMasks ::
		HeteroParList.PL SemaphorePipelineStageFlags sss,
	submitInfoCommandBuffers :: HeteroParList.PL (U2 CommandBuffer.Binded) svss,
	submitInfoSignalSemaphores ::
		HeteroParList.PL Semaphore.S ssss }

class M.SubmitInfoListToCore (MiddleNextList nsssvsss) => SubmitInfoListToMiddle
	(nsssvsss :: [(Type, [Type], [(Type, [Type])], [Type])]) where
	type MiddleNextList nsssvsss :: [Type]
	submitInfoListToMiddle ::
		HeteroParList.PL (U4 SubmitInfo) nsssvsss ->
		HeteroParList.PL M.SubmitInfo (MiddleNextList nsssvsss)

instance SubmitInfoListToMiddle '[] where
	type MiddleNextList '[] = '[]
	submitInfoListToMiddle HeteroParList.Nil = HeteroParList.Nil

instance (
	WithPoked n,
	SubmitInfoListToMiddle nssvsss ) =>
	SubmitInfoListToMiddle ('(n, sss, svss, ssss) ': nssvsss) where
	type MiddleNextList ('(n, sss, svss, ssss) ': nssvsss) =
		n ': MiddleNextList nssvsss
	submitInfoListToMiddle (U4 si :** sis) =
		submitInfoToMiddle si :** submitInfoListToMiddle sis

submitInfoToMiddle ::
	SubmitInfo n sss svss ssss -> M.SubmitInfo n
submitInfoToMiddle SubmitInfo {
	submitInfoNext = mnxt,
	submitInfoWaitSemaphoreDstStageMasks =
		semaphorePipelineStageFlagsToMiddle -> wsdsms,
	submitInfoCommandBuffers = HeteroParList.toList (\(U2 x) -> CommandBuffer.unBinded x) -> cbs,
	submitInfoSignalSemaphores =
		HeteroParList.toList (\(Semaphore.S s) -> s) -> ssmprs
	} = M.SubmitInfo {
	M.submitInfoNext = mnxt,
	M.submitInfoWaitSemaphoreDstStageMasks = wsdsms,
	M.submitInfoCommandBuffers = cbs,
	M.submitInfoSignalSemaphores = ssmprs }

-- deriving instance (Show n, Show (HeteroParList SemaphorePipelineStageFlags sss)) =>
--	Show (SubmitInfo n sss s vs)

class SemaphorePipelineStageFlagsFromMiddle sss where
	semaphorePipelineStageFlagsFromMiddle ::
		[(Semaphore.M.S, Pipeline.StageFlags)] ->
		HeteroParList.PL SemaphorePipelineStageFlags sss

instance SemaphorePipelineStageFlagsFromMiddle '[] where
	semaphorePipelineStageFlagsFromMiddle [] = HeteroParList.Nil

instance SemaphorePipelineStageFlagsFromMiddle sss =>
	SemaphorePipelineStageFlagsFromMiddle (ss ': sss) where
	semaphorePipelineStageFlagsFromMiddle ((s, psfs) : spsfss) =
		SemaphorePipelineStageFlags (Semaphore.S s) psfs :**
		semaphorePipelineStageFlagsFromMiddle spsfss
