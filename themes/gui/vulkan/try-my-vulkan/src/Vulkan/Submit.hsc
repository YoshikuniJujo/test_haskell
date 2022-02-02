{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Submit where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.Kind
import Data.Word

import Vulkan.Base
import Vulkan.Exception
import Vulkan.Device
import Vulkan.Semaphore
import Vulkan.Fence
import Vulkan.PipelineStageFlagBits
import Vulkan.CommandBuffer

import qualified Vulkan.Submit.Internal as I

#include <vulkan/vulkan.h>

data Info n vtss = Info {
	infoNext :: Maybe n,
	infoWaitSemaphoreAndDstStageMasks :: [(Semaphore, PipelineStageFlags)],
	infoCommandBuffers :: CommandBufferList vtss,
	infoSignalSemaphores :: [Semaphore] }

data CommandBufferList (vtss :: [(Type, [Type])]) where
	CBNil :: CommandBufferList '[]
	(:+:) :: CommandBuffer vs ts -> CommandBufferList vtss ->
		CommandBufferList ('(vs, ts) ': vtss)

commandBufferListToList :: CommandBufferList vtss -> [Ptr CommandBufferTag]
commandBufferListToList CBNil = []
commandBufferListToList (CommandBuffer cbp :+: cbs) =
	cbp : commandBufferListToList cbs

infoToC :: Pointable n => Info n vtss -> ContT r IO I.Info
infoToC Info {
	infoNext = mnxt,
	infoWaitSemaphoreAndDstStageMasks = unzip -> (wsmps, psfs),
	infoCommandBuffers = commandBufferListToList -> cbs,
	infoSignalSemaphores = ssmps
	} = do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	wsmppsfc = length wsmps
	pwsmps <- ContT $ allocaArray wsmppsfc
	lift $ pokeArray pwsmps wsmps
	ppsfs <- ContT $ allocaArray wsmppsfc
	lift $ pokeArray ppsfs psfs
	let	cbc = length cbs
	pcbs <- ContT $ allocaArray cbc
	lift $ pokeArray pcbs cbs
	let	ssmpc = length ssmps
	pssmps <- ContT $ allocaArray ssmpc
	lift $ pokeArray pssmps ssmps
	pure I.Info {
		I.infoSType = (),
		I.infoPNext = pnxt,
		I.infoWaitSemaphoreCount = fromIntegral wsmppsfc,
		I.infoPWaitSemaphores = pwsmps,
		I.infoPWaitDstStageMask = ppsfs,
		I.infoCommandBufferCount = fromIntegral cbc,
		I.infoPCommandBuffers = pcbs,
		I.infoSignalSemaphoreCount = fromIntegral ssmpc,
		I.infoPSignalSemaphores = pssmps }

queue :: Pointable n => Queue -> [Info n vtss] -> Fence -> IO ()
queue q is f = ($ pure) $ runContT do
	let	ic = length is
	iis <- infoToC `mapM` is
	piis <- ContT $ allocaArray ic
	lift $ pokeArray piis iis
	lift do	r <- c_vkQueueSubmit q (fromIntegral ic) piis f
		throwUnlessSuccess r

foreign import ccall "vkQueueSubmit" c_vkQueueSubmit ::
	Queue -> #{type uint32_t} -> Ptr I.Info -> Fence -> IO Result
