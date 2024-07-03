{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.CanvasContext.Rendering2d where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.CanvasContext qualified as JS.CanvasContext
import GHC.JS.Value.CanvasContext.Rendering2d.Path qualified as Path
import GHC.JS.Value.CanvasContext.Rendering2d.Pathable qualified as Pathable

newtype R = R JSVal

instance JS.Value.IsJSVal R where toJSVal (R v) = v

instance JS.Value.V R where
	toV = JS.CanvasContext.toV; fromV = JS.CanvasContext.fromV

instance JS.Object.IsO R

instance JS.CanvasContext.IsC R where
	downCheck x = JS.Object.toO x `JS.Object.isInstanceOf` rClass
	downMake = R

instance Pathable.IsP R

rClass :: JS.Object.Class
rClass = JS.Object.Class js_CanvasRenderingContext2D

foreign import javascript "(() => { return CanvasRenderingContext2D; })"
	js_CanvasRenderingContext2D :: JSVal

fillRect :: R -> Double -> Double -> Double -> Double -> IO ()
fillRect (R ctx) = js_fillRect ctx

foreign import javascript "((ctx, l, t, w, h) => { ctx.fillRect(l, t, w, h); })"
	js_fillRect :: JSVal -> Double -> Double -> Double -> Double -> IO ()

fill :: R -> Maybe Path.P -> Maybe FillRule -> IO ()
fill (R cxt) mp mfr = case (mp, mfr) of
	(Nothing, Nothing) -> js_fill cxt
	(Just (Path.P p), Nothing) -> js_fill_path cxt p
	(Nothing, Just (FillRule fr)) -> js_fill_fillrule cxt fr
	(Just (Path.P p), Just (FillRule fr)) -> js_fill_path_fillrule cxt p fr

foreign import javascript "((ctx) => { ctx.fill(); })" js_fill :: JSVal -> IO ()

foreign import javascript "((ctx, p) => { ctx.fill(p); })"
	js_fill_path :: JSVal -> JSVal -> IO ()

foreign import javascript "((ctx, fr) => { ctx.fill(fr); })"
	js_fill_fillrule :: JSVal -> JSVal -> IO ()

foreign import javascript "((ctx, p, fr) => { ctx.fill(p, fr); })"
	js_fill_path_fillrule :: JSVal -> JSVal -> JSVal -> IO ()

newtype FillRule = FillRule JSVal

nonzero, evenodd :: FillRule
nonzero = FillRule js_nonzero
evenodd = FillRule js_evenodd

foreign import javascript "(() => { return nonzero; })" js_nonzero :: JSVal
foreign import javascript "(() => { return evenodd; })" js_evenodd :: JSVal

stroke :: R -> Maybe Path.P -> IO ()
stroke (R ctx) = \case
	Nothing -> js_stroke ctx; Just (Path.P p) -> js_stroke_path ctx p

foreign import javascript "((ctx) => { ctx.stroke(); })"
	js_stroke :: JSVal -> IO ()

foreign import javascript "((ctx, p) => { ctx.stroke(p); })"
	js_stroke_path :: JSVal -> JSVal -> IO ()
