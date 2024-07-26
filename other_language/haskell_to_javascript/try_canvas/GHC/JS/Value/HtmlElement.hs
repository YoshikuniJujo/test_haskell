{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlElement where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Element qualified as JS.Element
import Data.Typeable
import Data.Maybe

data H = forall he . JS.Value.V he => H he

instance JS.Value.IsJSVal H where toJSVal (H he) = JS.Value.toJSVal he
instance JS.Value.V H where toV = JS.Element.toValue; fromV = JS.Element.fromValue

toV :: JS.Value.V he => he -> JS.Value.Some
toV = JS.Value.toV . H

fromV :: JS.Value.V he => JS.Value.Some -> Maybe he
fromV v = JS.Value.fromV v >>= \(H he) -> cast he

toH :: IsH h => h -> H
toH = fromJust . JS.Value.cast

fromH :: forall h . IsH h => H -> Maybe h
fromH he = case (JS.Value.cast he, downCheck @h he) of
	(Just d, _) -> Just d
	(_, True) -> Just . downMake $ JS.Value.toJSVal he
	_ -> Nothing

instance JS.Object.IsO H
instance JS.EventTarget.IsE H

instance JS.Node.IsN H where
	downCheck he = JS.Object.toO he `JS.Object.isInstanceOf` hClass
	downMake = H . OtherH

instance JS.Element.IsE H where
	downCheck he = JS.Object.toO he `JS.Object.isInstanceOf` hClass
	downMake = H . OtherH

hClass :: JS.Object.Class
hClass = JS.Object.Class js_HTMLElement

foreign import javascript "(() => { return HTMLElement; })"
	js_HTMLElement :: JSVal

class JS.Element.IsE he => IsH he where
	downCheck :: H -> Bool; downMake :: JSVal -> he

newtype OtherH = OtherH JSVal

instance JS.Value.IsJSVal OtherH where toJSVal (OtherH v) = v
instance JS.Value.V OtherH where toV = toV; fromV = fromV

instance JS.Object.IsO OtherH
instance JS.EventTarget.IsE OtherH

instance JS.Node.IsN OtherH where
	downCheck he = JS.Object.toO he `JS.Object.isInstanceOf` hClass
	downMake = OtherH

instance JS.Element.IsE OtherH where
	downCheck he = JS.Object.toO he `JS.Object.isInstanceOf` hClass
	downMake = OtherH

instance IsH OtherH where downCheck = const True; downMake = OtherH

getOffsetWidth :: H -> IO Double
getOffsetWidth = js_getOffsetWidth . JS.Value.toJSVal

foreign import javascript "((he) => { return he.offsetWidth; })"
	js_getOffsetWidth :: JSVal -> IO Double
