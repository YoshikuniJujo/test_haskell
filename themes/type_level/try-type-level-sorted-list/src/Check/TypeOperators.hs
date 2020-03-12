{-# LANGUAGE TypeOperators, NoStarIsType #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Check.TypeOperators where

type a + b = Either a b

type a * b = (a, b)
