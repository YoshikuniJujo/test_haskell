{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIOnS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Document where

import GHC.JS.Prim (JSVal, isNull, isUndefined, toJSString, fromJSString)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget
import GHC.JS.Value.Node qualified as JS.Node
import GHC.JS.Value.Element qualified as JS.Element

newtype D = D { unD :: JSVal }

instance Show D where
	show d = "(" ++ JS.Object.toString (JS.Object.toO d) ++ ")"

instance JS.Value.IsJSVal D where toJSVal (D v) = v
instance JS.Value.V D where toV = JS.Node.toValue; fromV = JS.Node.fromValue

instance JS.Object.IsO D
instance JS.EventTarget.IsE D

instance JS.Node.IsN D where
	downCheck nd = JS.Node.nodeType nd == JS.Node.DocumentNode
	downMake = D

---------------------------------------------------------------------------
--- INSTANCE PROPERTY                                                   ---
---------------------------------------------------------------------------

-- Document.documentURI

documentUri :: D -> String
documentUri = fromJSString . js_documentURI . unD

foreign import javascript "((d) => { return d.documentURI; })"
	js_documentURI :: JSVal -> JSVal

---------------------------------------------------------------------------
--- INSTANCE METHOD                                                     ---
---------------------------------------------------------------------------

-- Document.getElementById()

getElementById :: D -> String -> IO (Maybe JS.Element.E)
getElementById (D dc) (toJSString -> i) = js_getElementById dc i >>= \case
	e	| isNull e -> pure Nothing
		| isUndefined e ->
			error "Document.getElementById() return undefined"
		| otherwise ->
			pure . Just . JS.Element.toE $ JS.Element.OtherE e

foreign import javascript "((d, id) => { return d.getElementById(id); })"
	js_getElementById :: JSVal -> JSVal -> IO JSVal
