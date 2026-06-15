{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlElement (
	H, toValue, fromValue, toH, fromH, IsH(..), offsetWidth ) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Element qualified as JS.Element
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data H = forall he . JS.Value.V he => H he

instance JS.Value.IsJSVal H where toJSVal (H he) = JS.Value.toJSVal he
instance JS.Value.V H where toV = JS.Element.toValue; fromV = JS.Element.fromValue

toValue :: JS.Value.V he => he -> JS.Value.Some
toValue = JS.Value.toV . H

fromValue :: JS.Value.V he => JS.Value.Some -> Maybe he
fromValue v = JS.Value.fromV v >>= \(H he) -> cast he

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

toH :: IsH h => h -> H
toH = fromJust . JS.Value.cast

fromH :: forall h . IsH h => H -> Maybe h
fromH he = case (JS.Value.cast he, downCheck @h he) of
	(Just d, _) -> Just d
	(_, True) -> Just . downMake $ JS.Value.toJSVal he
	_ -> Nothing

class JS.Element.IsE he => IsH he where
	downCheck :: H -> Bool; downMake :: JSVal -> he

newtype OtherH = OtherH JSVal

instance JS.Value.IsJSVal OtherH where toJSVal (OtherH v) = v
instance JS.Value.V OtherH where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherH
instance JS.EventTarget.IsE OtherH

instance JS.Node.IsN OtherH where
	downCheck he = JS.Object.toO he `JS.Object.isInstanceOf` hClass
	downMake = OtherH

instance JS.Element.IsE OtherH where
	downCheck he = JS.Object.toO he `JS.Object.isInstanceOf` hClass
	downMake = OtherH

instance IsH OtherH where downCheck = const True; downMake = OtherH

---------------------------------------------------------------------------
--- INSTANCE PROPERTY
---------------------------------------------------------------------------

-- HTMLElement.offsetWidth

offsetWidth :: H -> IO Double
offsetWidth = js_offsetWidth . JS.Value.toJSVal

foreign import javascript "((he) => { return he.offsetWidth; })"
	js_offsetWidth :: JSVal -> IO Double
