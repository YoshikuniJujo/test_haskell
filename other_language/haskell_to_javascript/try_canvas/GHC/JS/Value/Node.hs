{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Node where

import GHC.JS.Prim (JSVal, isUndefined, isNull, fromJSString)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data N = forall n . JS.Value.V n => N n

instance JS.Value.IsJSVal N where toJSVal (N n) = JS.Value.toJSVal n

instance JS.Value.V N where
	toV = JS.EventTarget.toValue; fromV = JS.EventTarget.fromValue

instance JS.Object.IsO N
instance JS.EventTarget.IsE N

toValue :: JS.Value.V n => n -> JS.Value.Some
toValue = JS.Value.toV . N

fromValue :: JS.Value.V n => JS.Value.Some -> Maybe n
fromValue v = JS.Value.fromV v >>= \(N n) -> cast n

toN :: IsN n => n -> N; toN = fromJust . JS.Value.cast

fromN :: forall n . IsN n => N -> Maybe n
fromN nd = case (JS.Value.cast nd, downCheck @n nd) of
	(Just d, _) -> Just d
	(Nothing, True) -> Just . downMake $ JS.Value.toJSVal nd
	_ -> Nothing

class JS.EventTarget.IsE n => IsN n where
	downCheck :: N -> Bool; downMake :: JSVal -> n

newtype OtherN = OtherN JSVal

instance JS.Value.IsJSVal OtherN where toJSVal (OtherN v) = v
instance JS.Value.V OtherN where toV = toValue; fromV = fromValue

instance JS.Object.IsO OtherN
instance JS.EventTarget.IsE OtherN
instance IsN OtherN where downCheck = const True; downMake = OtherN

---------------------------------------------------------------------------
--- INSTANCE PROPERTY                                                   ---
---------------------------------------------------------------------------

-- Node.firstChild

firstChild :: N -> IO (Maybe N)
firstChild nd = js_firstChild (JS.Value.toJSVal nd) >>= \case
	c	| isNull c -> pure Nothing
		| isUndefined c -> error "Node.firstChild returned undefined"
		| otherwise -> pure . Just . toN $ OtherN c

foreign import javascript "((n) => { return n.firstChild; })"
	js_firstChild :: JSVal -> IO JSVal

-- Node.nodeName

nodeName :: N -> String
nodeName = fromJSString . js_nodeName . JS.Value.toJSVal

foreign import javascript "((n) => { return n.nodeName; })"
	js_nodeName :: JSVal -> JSVal

-- Node.nodeType

nodeType :: N -> NodeType
nodeType = NodeType . js_nodeType . JS.Value.toJSVal

foreign import javascript "((n) => { return n.nodeType; })"
	js_nodeType :: JSVal -> Word

newtype NodeType = NodeType Word deriving Eq

instance Show NodeType where
	show = \case
		ElementNode -> "ElementNode"
		TextNode -> "TextNode"
		DocumentNode -> "DocumentNode"
		NodeType nt -> "(NodeType " ++ show nt ++ ")"

pattern ElementNode :: NodeType
pattern ElementNode <- NodeType 1 where ElementNode = NodeType 1

pattern TextNode :: NodeType
pattern TextNode <- NodeType 3 where TextNode = NodeType 3

pattern DocumentNode :: NodeType
pattern DocumentNode <- NodeType 9 where DocumentNode = NodeType 9

-- Node.parentNode

parentNode :: N -> IO (Maybe N)
parentNode nd = js_parentNode (JS.Value.toJSVal nd) >>= \case
	p	| isNull p -> pure Nothing
		| isUndefined p -> error "Node.parentNode returned undefined"
		| otherwise -> pure . Just . toN $ OtherN p

foreign import javascript "((n) => { return n.parentNode; })"
	js_parentNode :: JSVal -> IO JSVal

---------------------------------------------------------------------------
--- INSTANCE METHOD                                                     ---
---------------------------------------------------------------------------

-- Node.appendChild()

appendChild :: N -> N -> IO ()
appendChild n c = js_appendChild (JS.Value.toJSVal n) (JS.Value.toJSVal c)

foreign import javascript "((n, c) => { n.appendChild(c); })"
	js_appendChild :: JSVal -> JSVal -> IO ()

-- Node.hasChildNodes()

hasChildNodes :: N -> IO Bool
hasChildNodes = js_hasChildNodes . JS.Value.toJSVal

foreign import javascript "((n) => { return n.hasChildNodes(); })"
	js_hasChildNodes :: JSVal -> IO Bool

-- Node.removeChild

removeChild :: N -> N -> IO N
removeChild p c =
	N . OtherN <$> js_removeChild (JS.Value.toJSVal p) (JS.Value.toJSVal c)

foreign import javascript "((n, c) => { n.removeChild(c); })"
	js_removeChild :: JSVal -> JSVal -> IO JSVal
