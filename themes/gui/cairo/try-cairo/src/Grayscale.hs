{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Grayscale where

reverseGamma :: Double -> Double
reverseGamma = (** 2.2)

gamma :: Double -> Double
gamma = (** (1 / 2.2))

grayscale :: Double -> Double -> Double -> Double
grayscale r g b = gamma $ 0.2126 * rv r + 0.7152 * rv g + 0.0722 * rv b
	where rv = reverseGamma
