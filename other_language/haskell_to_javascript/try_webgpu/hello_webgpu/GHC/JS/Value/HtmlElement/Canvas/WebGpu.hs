{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.HtmlElement.Canvas.WebGpu where

import GHC.JS.Value.HtmlElement.Canvas qualified as JS.HtmlCanvasElement

pattern ContextTypeWebGpu :: JS.HtmlCanvasElement.ContextType
pattern ContextTypeWebGpu = JS.HtmlCanvasElement.ContextType "webgpu"
