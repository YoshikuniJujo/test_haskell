{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Rendering2d.Path where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.CanvasContext.Rendering2d.Pathable qualified as JS.Pathable

newtype P = P JSVal

instance JS.Value.IsJSVal P where toJSVal (P p) = p
instance JS.Value.V P where toV = JS.Object.toV; fromV = JS.Object.fromV

instance JS.Object.IsO P
instance JS.Pathable.IsP P

newFromScratch :: IO P
newFromScratch = P <$> js_newPath2D

foreign import javascript "(() => { return new Path2D(); })"
	js_newPath2D :: IO JSVal

addPathNoTransform :: P -> P -> IO ()
addPathNoTransform (P pd) (P ps) = js_addPath pd ps

foreign import javascript "((pd, ps) => { pd.addPath(ps); })"
	js_addPath :: JSVal -> JSVal -> IO ()

foreign import javascript "((pd, ps, tr) => { pd.addPath(ps, tr); })"
	js_addPath_transform :: JSVal -> JSVal -> JSVal -> IO ()
