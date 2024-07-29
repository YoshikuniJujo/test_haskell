{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Event.Mouse (
	M, toValue, fromValue, toM, IsM, offsetX, offsetY
	) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Event qualified as JS.Event
import GHC.JS.Value.Event.UI qualified as JS.UIEvent
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data M = forall ms . JS.Value.V ms => M ms

instance JS.Value.IsJSVal M where toJSVal (M ms) = JS.Value.toJSVal ms

instance JS.Value.V M where
	toV = JS.UIEvent.toValue; fromV = JS.UIEvent.fromValue

toValue :: JS.Value.V ms => ms -> JS.Value.Some
toValue = JS.Value.toV . M

fromValue :: JS.Value.V ms => JS.Value.Some -> Maybe ms
fromValue v = JS.Value.fromV v >>= \(M ms) -> cast ms

instance JS.Object.IsO M

instance JS.Event.IsE M where
	downCheck ev = JS.Object.toO ev `JS.Object.isInstanceOf` mClass
	downMake = M . OtherM

toM :: IsM ms => ms -> M
toM = fromJust . JS.Value.cast

class JS.UIEvent.IsU ms => IsM ms

mClass :: JS.Object.Class
mClass = JS.Object.Class js_MouseEvent

foreign import javascript "(() => { return MouseEvent; })"
	js_MouseEvent :: JSVal

newtype OtherM = OtherM JSVal

instance JS.Value.IsJSVal OtherM where toJSVal (OtherM v) = v
instance JS.Value.V OtherM where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherM

instance JS.Event.IsE OtherM where
	downCheck ev = JS.Object.toO ev `JS.Object.isInstanceOf` mClass
	downMake = OtherM

---------------------------------------------------------------------------
--- INSTANCE PROPERTY
---------------------------------------------------------------------------

-- MouseEvent.offsetX and MouseEvent.offsetY

offsetX, offsetY :: M -> IO Double
offsetX = js_offsetX . JS.Value.toJSVal
offsetY = js_offsetY . JS.Value.toJSVal

foreign import javascript "((e) => { return e.offsetX; })"
	js_offsetX :: JSVal -> IO Double

foreign import javascript "((e) => { return e.offsetY; })"
	js_offsetY :: JSVal -> IO Double
