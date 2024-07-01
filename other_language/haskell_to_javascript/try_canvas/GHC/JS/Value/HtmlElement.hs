{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlElement where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Element qualified as JS.Element
import Data.Typeable

data H = forall he . JS.Value.V he => H he

instance JS.Value.IsJSVal H where toJSVal (H he) = JS.Value.toJSVal he
instance JS.Value.V H where toV = JS.Element.toV; fromV = JS.Element.fromV

toV :: JS.Value.V he => he -> JS.Value.Some
toV = JS.Value.toV . H

fromV :: JS.Value.V he => JS.Value.Some -> Maybe he
fromV v = JS.Value.fromV v >>= \(H he) -> cast he

instance JS.Object.IsO H
instance JS.EventTarget.IsE H
