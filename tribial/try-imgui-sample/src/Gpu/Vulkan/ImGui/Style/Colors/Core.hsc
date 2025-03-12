{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.ImGui.Style.Colors.Core (

	darkNoArg, lightNoArg, classicNoArg

	) where

darkNoArg, lightNoArg, classicNoArg :: IO ()
darkNoArg = cxx_style_colors_dark_no_arg
lightNoArg = cxx_style_colors_light_no_arg
classicNoArg = cxx_style_colors_classic_no_arg

foreign import ccall "style_colors_dark_no_arg"
	cxx_style_colors_dark_no_arg :: IO ()

foreign import ccall "style_colors_light_no_arg"
	cxx_style_colors_light_no_arg :: IO ()

foreign import ccall "style_colors_classic_no_arg"
	cxx_style_colors_classic_no_arg :: IO ()
