{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Node where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import Data.Typeable
import Data.Maybe

data N = forall n . JS.Value.V n => N n

instance JS.Value.IsJSVal N where toJSVal (N n) = JS.Value.toJSVal n

instance JS.Value.V N where
	toV = JS.EventTarget.toV; fromV = JS.EventTarget.fromV

toV :: JS.Value.V n => n -> JS.Value.Some
toV = JS.Value.toV . N

fromV :: JS.Value.V n => JS.Value.Some -> Maybe n
fromV v = JS.Value.fromV v >>= \(N n) -> cast n

class JS.EventTarget.IsE n => IsN n where
	toN :: n -> N; toN = fromJust . JS.Value.cast
	downCheck :: N -> Bool; downMake :: JSVal -> n

fromN :: forall n . IsN n => N -> Maybe n
fromN nd = case (JS.Value.cast nd, downCheck @n nd) of
	(Just d, _) -> Just d
	(Nothing, True) -> Just . downMake $ JS.Value.toJSVal nd
	_ -> Nothing

getNodeName :: N -> String
getNodeName nd = fromJSString . js_getNodeName $ JS.Value.toJSVal nd

foreign import javascript "((n) => { return n.nodeName; })"
	js_getNodeName :: JSVal -> JSVal

getNodeType :: N -> NodeType
getNodeType nd = NodeType . js_getNodeType $ JS.Value.toJSVal nd

newtype NodeType = NodeType Word deriving Eq

instance Show NodeType where
	show = \case
		DocumentNode -> "DocumentNode"
		NodeType nt -> "(NodeType " ++ show nt ++ ")"

foreign import javascript "((n) => { return n.nodeType; })"
	js_getNodeType :: JSVal -> Word

pattern ElementNode :: NodeType
pattern ElementNode <- NodeType 1 where ElementNode = NodeType 1

pattern DocumentNode :: NodeType
pattern DocumentNode <- NodeType 9 where DocumentNode = NodeType 9

newtype OtherN = OtherN JSVal

instance JS.Value.IsJSVal OtherN where toJSVal (OtherN v) = v
instance JS.Value.V OtherN where toV = toV; fromV = fromV

instance JS.Object.IsO OtherN
instance JS.EventTarget.IsE OtherN
instance IsN OtherN where downCheck = const True; downMake = OtherN

firstChild :: N -> Maybe N
firstChild nd
	| isNull c = Nothing
	| isUndefined c = error "Node.firstChild return undefined"
	| otherwise = Just . toN $ OtherN c
	where c = js_firstChild $ JS.Value.toJSVal nd

foreign import javascript "((n) => { return n.firstChild; })"
	js_firstChild :: JSVal -> JSVal

parentNode :: N -> Maybe N
parentNode nd
	| isNull p = Nothing
	| isUndefined p = error "Node.parentNode return undefined"
	| otherwise = Just . toN $ OtherN p
	where p = js_parentNode $ JS.Value.toJSVal nd

foreign import javascript "((n) => {return n.parentNode; })"
	js_parentNode :: JSVal -> JSVal
