{-# LANGUAGE BlockArguments #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Document where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Element qualified as JS.Element

newtype D = D JSVal

instance JS.Value.IsJSVal D where toJSVal (D v) = v
instance JS.Value.V D where toV = JS.Node.toV; fromV = JS.Node.fromV

getDocumentURI :: D -> String
getDocumentURI (D dc) = fromJSString $ js_getDocumentURI dc

foreign import javascript "((d) => { return d.documentURI; })"
	js_getDocumentURI :: JSVal -> JSVal

instance JS.Object.IsO D
instance JS.EventTarget.IsE D

instance JS.Node.IsN D where
	downCheck nd = JS.Node.getNodeType nd == JS.Node.DocumentNode
	downMake = D

instance Show D where
	show (D dc) = "(" ++ fromJSString (js_toString dc) ++ ")"

foreign import javascript "((v) => { return v.toString(); })"
	js_toString :: JSVal -> JSVal

getElementById :: D -> String -> IO (Maybe JS.Element.E)
getElementById (D dc) i = do
	e <- js_getElementById dc (toJSString i)
	case e of
		_	| isNull e -> pure Nothing
			| isUndefined e -> error
				"Document.getElementById return undefined"
			| otherwise -> pure
				. Just . JS.Element.toE $ JS.Element.OtherE e

foreign import javascript "((d, id) => { return d.getElementById(id); })"
	js_getElementById :: JSVal -> JSVal -> IO JSVal
