{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CharacterData (
	C, toValue, fromValue, toC, fromC, IsC(..) ) where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data C = forall cd . JS.Value.V cd => C cd

instance JS.Value.IsJSVal C where toJSVal (C cd) = JS.Value.toJSVal cd
instance JS.Value.V C where toV = JS.Node.toValue; fromV = JS.Node.fromValue

toValue :: JS.Value.V cd => cd -> JS.Value.Some
toValue = JS.Value.toV . C

fromValue :: JS.Value.V cd => JS.Value.Some -> Maybe cd
fromValue v = JS.Value.fromV v >>= \(C cd) -> cast cd

instance JS.Object.IsO C
instance JS.EventTarget.IsE C

instance JS.Node.IsN C where
	downCheck nd = JS.Object.toO nd `JS.Object.isInstanceOf` cClass
	downMake = C . OtherC

toC :: IsC c => c -> C
toC = fromJust . JS.Value.cast

fromC :: forall c . IsC c => C -> Maybe c
fromC cd = case (JS.Value.cast cd, downCheck @c cd) of
	(Just cd', _) -> Just cd'
	(_, True) -> Just . downMake $ JS.Value.toJSVal cd
	_ -> Nothing

class JS.Node.IsN c => IsC c where
	downCheck :: C -> Bool; downMake :: JSVal -> c

newtype OtherC = OtherC JSVal

instance JS.Value.IsJSVal OtherC where toJSVal (OtherC v) = v
instance JS.Value.V OtherC where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherC
instance JS.EventTarget.IsE OtherC

instance JS.Node.IsN OtherC where
	downCheck nd = JS.Object.toO nd `JS.Object.isInstanceOf` cClass
	downMake = OtherC

cClass :: JS.Object.Class
cClass = JS.Object.Class js_CharacterData

foreign import javascript "(() => { return CharacterData; })"
	js_CharacterData :: JSVal
