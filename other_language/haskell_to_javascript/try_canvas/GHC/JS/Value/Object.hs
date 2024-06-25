{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Object where

import Data.Typeable

import GHC.JS.Value qualified as JS.Value

data O = forall o . JS.Value.V o => O { unO :: o }

instance JS.Value.IsJSVal O where toJSVal (O o) = JS.Value.toJSVal o

instance JS.Value.V O

toV :: JS.Value.V o => o -> JS.Value.Some
toV = JS.Value.toV . O

fromV :: JS.Value.V o => JS.Value.Some -> Maybe o
fromV v = do
	O o <- JS.Value.fromV v
	cast o
