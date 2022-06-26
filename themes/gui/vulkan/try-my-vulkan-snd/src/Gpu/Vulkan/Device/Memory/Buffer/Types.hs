module Gpu.Vulkan.Device.Memory.Buffer.Types where

import Data.Kind.Object
import Data.HeteroList

import qualified Gpu.Vulkan.Device.Middle as Device.M

data Form objs = Form {
	formOffset :: Device.M.Size,
	formRange :: Device.M.Size,
	formObjects :: HeteroVarList ObjectLength objs }
