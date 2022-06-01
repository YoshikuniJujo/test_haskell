{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Vulkan.Queue where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont

import Vulkan
import Vulkan.Exception
import Vulkan.Exception.Enum

import {-# SOURCE #-} qualified Vulkan.Fence as Fence

import qualified Vulkan.Queue.Core as C

newtype Queue = Queue C.Q deriving Show

queueSubmit :: Pointable n => Queue -> [SubmitInfo n vs] -> Maybe Fence.F -> IO ()
queueSubmit (Queue q)
	(length &&& id -> (sic, sis)) f = ($ pure) $ runContT do
	csis <- submitInfoToCore `mapM` sis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.queueSubmit q (fromIntegral sic) psis
			$ Fence.maybeFToCore f
		throwUnlessSuccess $ Result r

queueWaitIdle :: Queue -> IO ()
queueWaitIdle (Queue q) = throwUnlessSuccess . Result =<< C.queueWaitIdle q
