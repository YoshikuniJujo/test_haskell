{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Function.ToolsYj (const2, const3, const4, const5) where

const2 :: a -> b -> c -> a
const2 = const . const

const3 :: a -> b -> c -> d -> a
const3 = const . const2

const4 :: a -> b -> c -> d -> e -> a
const4 = const . const3

const5 :: a -> b -> c -> d -> e -> f -> a
const5 = const . const4
