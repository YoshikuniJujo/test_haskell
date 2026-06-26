module GHC.JS.Value.GpuOverridableConstant where

import Data.Int

type Key = Either Int String

data Value
	= Bool Bool
	| I32 Int32

