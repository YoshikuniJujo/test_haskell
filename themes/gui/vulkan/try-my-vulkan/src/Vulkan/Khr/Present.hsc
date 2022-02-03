{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Khr.Present where

import Foreign.Ptr
import Foreign.Marshal.Array
import Control.Monad.Cont
import Data.Word

import Vulkan.Base
import Vulkan.Semaphore

import qualified Vulkan.Khr.Present.Internal as I
import qualified Vulkan.Khr.Swapchain.Internal as I

#include <vulkan/vulkan.h>

data Info n = Info {
	infoNext :: Maybe n,
	infoWaitSemaphores :: [Semaphore],
	infoSwapchainImageIndices :: [(I.Swapchain, #{type uint32_t})] }
	deriving Show

infoToC :: Pointable n => Info n -> ContT r IO I.Info
infoToC Info {
	infoNext = mnxt,
	infoWaitSemaphores = wss,
	infoSwapchainImageIndices = unzip -> (scs, iis) }= do
	(castPtr -> pnxt) <- ContT $ withMaybePointer mnxt
	let	wsc = length wss
	pwss <- ContT $ allocaArray wsc
	lift $ pokeArray pwss wss
	let	scc = length scs
	pscs <- ContT $ allocaArray scc
	piis <- ContT $ allocaArray scc
	lift do	pokeArray pscs scs
		pokeArray piis iis
	prslts <- ContT $ allocaArray scc
	pure I.Info {
		I.infoSType = (),
		I.infoPNext = pnxt,
		I.infoWaitSemaphoreCount = fromIntegral wsc,
		I.infoPWaitSemaphores = pwss,
		I.infoSwapchainCount = fromIntegral scc,
		I.infoPSwapchains = pscs,
		I.infoPImageIndices = piis,
		I.infoPResults = prslts }
