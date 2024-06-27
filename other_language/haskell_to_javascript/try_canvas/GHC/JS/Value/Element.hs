{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Element where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Node qualified as JS.Node
import Data.Typeable

data E = forall em . JS.Value.V em => E em

instance JS.Value.IsJSVal E where toJSVal (E em) = JS.Value.toJSVal em
instance JS.Value.V E where toV = JS.Node.toV; fromV = JS.Node.fromV

toV :: JS.Value.V em => em -> JS.Value.Some
toV = JS.Value.toV . E

fromV :: JS.Value.V em => JS.Value.Some -> Maybe em
fromV v = JS.Value.fromV v >>= \(E em) -> cast em
