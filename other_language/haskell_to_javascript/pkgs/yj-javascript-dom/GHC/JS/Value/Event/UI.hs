{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Event.UI (U, toValue, fromValue, toU, IsU) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Event qualified as JS.Event
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data U = forall ui . JS.Value.V ui => U ui

instance JS.Value.IsJSVal U where toJSVal (U ui) = JS.Value.toJSVal ui
instance JS.Value.V U where toV = JS.Event.toValue; fromV = JS.Event.fromValue

toValue :: JS.Value.V ui => ui -> JS.Value.Some
toValue = JS.Value.toV . U

fromValue :: JS.Value.V ui => JS.Value.Some -> Maybe ui
fromValue v = JS.Value.fromV v >>= \(U ui) -> cast ui

instance JS.Object.IsO U

instance JS.Event.IsE U where
	downCheck ev = JS.Object.toO ev `JS.Object.isInstanceOf` uClass
	downMake = U . OtherU

toU :: IsU ui => ui -> U
toU = fromJust . JS.Value.cast

class JS.Event.IsE ui => IsU ui

uClass :: JS.Object.Class
uClass = JS.Object.Class js_UIEvent

foreign import javascript "(() => { return UIEvent; })" js_UIEvent :: JSVal

newtype OtherU = OtherU JSVal

instance JS.Value.IsJSVal OtherU where toJSVal (OtherU v) = v
instance JS.Value.V OtherU where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherU

instance JS.Event.IsE OtherU where
	downCheck ev = JS.Object.toO ev `JS.Object.isInstanceOf` uClass
	downMake = OtherU

instance IsU OtherU
