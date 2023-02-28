{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.Queue.Middle (

	-- * Type

	Q,

	-- * Submit and WaitIdle

	submit, waitIdle ) where

import Gpu.Vulkan.Queue.Middle.Internal
