{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Graphics.Rendering.Chart.Easy

signal :: [Double] -> [(Double, Double)]
signal xs = [ (x, (sin (x * pi / 45) + 1) / 2 * sin (x * pi / 5)) | x <- xs ]

chart :: EC (Layout Double Double) ()
chart = do
	layout_title .= "Amplitude Modulation"
	plot $ line "am" [signal [0, 0.5 .. 400]]
	plot $ points "am points" $ signal [0, 7 .. 400]

mkChart :: String -> String -> [(Double, Double)] -> EC (Layout Double Double) ()
mkChart ttl lt ps = do
	layout_title .= ttl
	plot $ line lt [ps]
