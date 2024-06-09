module Callback where

import Prelude
import Effect

foreign import app :: forall a b . (a -> b) -> a -> b

foreign import appEff0 :: forall a b . (a -> Effect b) -> a -> Effect b

foreign import appEff :: forall a b . (a -> Effect b) -> a -> Effect b
