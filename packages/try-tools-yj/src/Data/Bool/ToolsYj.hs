{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bool.ToolsYj (onlyIf) where

import Data.Bool

onlyIf :: (a -> Bool) -> a -> Maybe a
onlyIf p x = bool Nothing (Just x) (p x)
