{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue where

import Foreign.Marshal.Array
import Foreign.Pointable
import Control.Arrow
import Control.Monad.Cont

import Gpu.Vulkan
import Gpu.Vulkan.Exception
import Gpu.Vulkan.Exception.Enum

import {-# SOURCE #-} qualified Gpu.Vulkan.Fence.Middle as Fence

import qualified Gpu.Vulkan.Middle as M
import qualified Gpu.Vulkan.Queue.Core as C

newtype Q = Q C.Q deriving Show

submit :: Pointable n => Q -> [SubmitInfo n s vs] -> Maybe Fence.F -> IO ()
submit (Q q)
	(length &&& id -> (sic, sis)) f = ($ pure) $ runContT do
	csis <- (M.submitInfoToCore . submitInfoToMiddle) `mapM` sis
	psis <- ContT $ allocaArray sic
	lift do	pokeArray psis csis
		r <- C.submit q (fromIntegral sic) psis
			$ Fence.maybeFToCore f
		throwUnlessSuccess $ Result r

submit' :: Pointable n => Q -> [M.SubmitInfo n vs] -> Maybe Fence.F -> IO ()
submit' q = submit q . (submitInfoFromMiddle <$>)

waitIdle :: Q -> IO ()
waitIdle (Q q) = throwUnlessSuccess . Result =<< C.waitIdle q
