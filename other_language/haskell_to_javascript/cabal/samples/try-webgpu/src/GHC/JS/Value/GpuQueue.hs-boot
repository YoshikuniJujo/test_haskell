module GHC.JS.Value.GpuQueue where

import GHC.JS.Prim (JSVal)

newtype G = G { unG :: JSVal }
