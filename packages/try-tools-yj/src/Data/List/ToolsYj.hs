{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.ToolsYj (elemAll) where

import Data.List ((\\))

elemAll :: Eq a => [a] -> [a] -> Bool
elemAll es = null . (es \\)
